{
  description = "Dgram flake";
  inputs.nixpkgs.url = "nixpkgs";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
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
      pandocScriptBuilder = pkgs: pkgs.stdenv.mkDerivation {
        name = "pandoc-script";
        src = ./pandoc/dgram.lua;
        phases = [ "installPhase" ];
        installPhase = ''
          mkdir -p $out
          ls -l $src
          substitute $src $out/dgram.lua \
              --replace @ddgram@ ${pkgs.thisPackage}
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
              testFlags = ["-t 3"];
            })
            )).overrideAttrs (attrs: {
              nativeBuildInputs = attrs.nativeBuildInputs; # ++ [final.breakpointHook];
              checkPhase = ''
                export FONTCONFIG_FILE=${fonts final}
                export HOME=$(readlink -f ".");
                ${attrs.checkPhase}
              '';
          });
        };
      packages = forAllSystems (system: rec {
        thisPackage = nixpkgsFor.${system}.thisPackage;
        pandocScript = pandocScriptBuilder nixpkgsFor.${system}; 
        default = thisPackage;
      });
      apps = forAllSystems (system: rec {
        ddgram = {
          type = "app";
          program = "${nixpkgsFor.${system}.thisPackage}/bin/ddgram";
        };
        default = ddgram;
      });
      checks = self.packages;
      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.thisPackage];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            cabal-install
          ] ++ extraBuildInputs (nixpkgsFor.${system});
          shellHook = ''
          export FONTCONFIG_FILE=${fonts (nixpkgsFor.${system})}
          hpack
          '';
        });
  };
}
