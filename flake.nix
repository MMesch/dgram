{
  description = "simple Haskell flake";
  inputs.nixpkgs.url = "nixpkgs";
  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        config = {allowBroken = true;};
        overlays = [ self.overlay ];
      });
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
          final.haskell.lib.overrideCabal mypkg (old: {
            extraLibraries = extraBuildInputs final;
            testFlags = ["-t 1"];
            });
        };
      packages = forAllSystems (system: {
         thisPackage = nixpkgsFor.${system}.thisPackage;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.thisPackage);
      apps = forAllSystems (system: {
        ddgram = {
          type = "app";
          program = "${nixpkgsFor.${system}.thisPackage}/bin/ddgram";
        };
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
          shellHook = ''hpack'';
        });
  };
}
