{
  description = "Dgram flake";
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };
  outputs = { self, nixpkgs, flake-compat }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        config = {allowBroken = true;};
        overlays = [ self.overlay ];
      });
      pandocScriptBuilder = dgram: pkgs: pkgs.stdenv.mkDerivation {
        name = "pandoc-script";
        src = ./pandoc/dgram.lua;
        phases = [ "installPhase" ];
        installPhase = ''
          mkdir -p $out
          ls -l $src
          substitute $src $out/dgram.lua \
              --replace @dgram@ ${dgram}
          '';
      };
      fonts = pkgs: pkgs.makeFontsConf { fontDirectories = [ pkgs.dejavu_fonts ]; };
      extraBuildInputs = pkgs : with pkgs; [
            librsvg
            nodePackages.vega-cli
            # the following is waiting on https://github.com/NixOS/nixpkgs/pull/162434
            (nodePackages.vega-lite.override {
                postInstall = ''
                    cd node_modules
                    for dep in ${nodePackages.vega-cli}/lib/node_modules/vega-cli/node_modules/*; do
                      if [[ ! -d ''${dep##*/} ]]; then
                        ln -s "${nodePackages.vega-cli}/lib/node_modules/vega-cli/node_modules/''${dep##*/}"
                      fi
                    done
                  '';})
            nodePackages.mermaid-cli
            graphviz
            plantuml
            svgbob
      ];
    in
    {
      overlay = final: prev: {
        thisPackage =
          let
            mypkg = (final.haskellPackages.callCabal2nixWithOptions "lib" ./. "--hpack" {});
          in
          (
            final.haskell.lib.dontCheck (final.haskell.lib.overrideCabal mypkg (old: {
              extraLibraries = extraBuildInputs final;
              executableSystemDepends = extraBuildInputs final;
              testFlags = ["-t 3"];
            })
            )).overrideAttrs (attrs: {
              nativeBuildInputs = attrs.nativeBuildInputs; # ++ [final.breakpointHook];
              propagatedBuildInputs = attrs.propagatedBuildInputs ++ [(extraBuildInputs final)];
              checkPhase = ''
                export FONTCONFIG_FILE=${fonts final}
                export HOME=$(readlink -f ".");
                ${attrs.checkPhase}
              '';
          });
        };
      packages = forAllSystems (system: rec {
        dgramRaw = nixpkgsFor.${system}.thisPackage;
        dgramWrapped = nixpkgsFor.${system}.writeShellApplication {
          name = "dgram";
          runtimeInputs = extraBuildInputs nixpkgsFor.${system};
          text = ''
            echo "running wrapped dgram"
            ${dgramRaw}/bin/dgram "$@"
            '';
        };
        pandocScript = pandocScriptBuilder dgramWrapped nixpkgsFor.${system};
        default = dgramWrapped;
      });
      apps = forAllSystems (system: rec {
        dgram = {
          type = "app";
          program = "${self.packages.${system}.dgramWrapped}/bin/dgram";
        };
        default = dgram;
      });
      checks = self.packages;
      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.dgramRaw];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            cabal-install
          ] ++ extraBuildInputs (nixpkgsFor.${system});
          shellHook = ''
          hpack
          '';
        });
  };
}
