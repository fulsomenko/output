{
  description = "Output - Korean Learning App with Haskell";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;

        output = haskellPackages.callCabal2nix "output" ./. {};
      in
      {
        packages = {
          default = output;
          output = output;
        };

        apps = {
          default = {
            type = "app";
            program = "${output}/bin/output";
          };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            ghc
            cabal-install
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.stylish-haskell
            haskellPackages.hasktags

            git
            vim
            curl

            pkg-config
          ];

          shellHook = ''
            echo "Output - Korean Learning App"
            echo "Haskell environment loaded"
            echo "Available commands:"
            echo "  cabal build      - Build the project"
            echo "  cabal test       - Run tests"
            echo "  cabal repl       - Start REPL"
            echo "  cabal run output - Run the application"
          '';
        };

        devShell = self.devShells.${system}.default;
      }
    );
}
