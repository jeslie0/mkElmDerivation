{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let pkgs = nixpkgs.legacyPackages.${system};
          haskellPackages = pkgs.haskellPackages;
          packageName = with builtins;
            let cabalFileName = head (filter (name:
                  let len = builtins.stringLength name;
                  in len >= 6 && (substring (len - 6) 6 name == ".cabal")) (attrNames (readDir ./.)));
            in head (match "^.*name\:\ *([^[:space:]]*).*$" (readFile "${./.}\/${cabalFileName}"));
      in
        {
          packages.${packageName} = haskellPackages.callCabal2nix packageName self {};

          packages.test = pkgs.stdenv.mkDerivation {
            name = "test";
            src = pkgs.fetchurl {
              url ="https://github.com/0ui/elm-task-parallel/archive/refs/tags/2.0.0.tar.gz";
              sha256 = "7e1d8f6751a39e07937bc9961252d4c3948787e49d0832949a4b6e5ced85bcd9";
            };

          };
          defaultPackage = self.packages.${system}.${packageName};

          devShell = haskellPackages.shellFor {
            packages = p: [ self.defaultPackage.${system} ]; # This automatically pulls cabal libraries into the devshell, so they can be used in ghci
            buildInputs = with haskellPackages;
              [ ghc
                haskell-language-server
                cabal-install
                apply-refact
                hlint
                stylish-haskell
                hasktags
                hindent
              ];

            # Add build inputs of the following derivations.
            inputsFrom = [ ];

            # Enables Hoogle for the builtin packages.
            withHoogle = true;
          };
        }
    );
}
