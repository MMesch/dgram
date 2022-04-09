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
    in
    {
      overlay = (final: prev: {
        thisPackage = final.haskellPackages.callCabal2nixWithOptions "lib" ./. "--hpack" {};
      });
      packages = forAllSystems (system: {
         thisPackage = nixpkgsFor.${system}.thisPackage;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.thisPackage);
      checks = self.packages;
      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskellPackages;
            nodePackages = nixpkgsFor.${system}.nodePackages;
        in haskellPackages.shellFor {
          packages = p: [self.packages.${system}.thisPackage];
          withHoogle = true;
          buildInputs = with haskellPackages; [
            haskell-language-server
            cabal-install
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
            nixpkgsFor.${system}.graphviz
          ];
          shellHook = ''hpack'';
        });
  };
}
