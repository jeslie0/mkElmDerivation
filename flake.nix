{
  description = "A flake giving mkElmDerivation and a programme to fetch and hash Elm Packages.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    nix-filter.url = "github:numtide/nix-filter";
  };

  outputs = { self, nixpkgs, flake-utils, nix-filter }:
    { elmSrcsFunc = with builtins;
          elmJson:
          let
            elmData = fromJSON (readFile ./elmData.json);
            getHash = name: ver: elmData.${name}.${ver};
            elmDepDir = (fromJSON (readFile elmJson)).dependencies.direct;
            elmDepIndir = (fromJSON (readFile elmJson)).dependencies.indirect;
            elmDepTestDir = (fromJSON (readFile elmJson)).test-dependencies.direct;
            elmDepTestIndir = (fromJSON (readFile elmJson)).test-dependencies.indirect;
            attrFunc = attr: mapAttrs (name: value: { sha256 = getHash name value; version = value; }) attr;
          in
          attrFunc elmDepDir //
          attrFunc elmDepIndir //
          attrFunc elmDepTestDir //
          attrFunc elmDepTestIndir;

      mkElmDerivation = import ./mkElmDerivation.nix self.outputs.elmSrcsFunc;
    } //
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
        packageName = with builtins;
          let cabalFileName = head (filter
            (name: pkgs.lib.hasSuffix ".cabal" name)
            (attrNames (readDir ./.)));
          in head (match "^.*name\:\ *([^[:space:]]*).*$" (readFile "${./.}\/${cabalFileName}"));
      in
      {
        packages.${packageName} =
          let src = nix-filter.lib.filter {
                root = ./.;
                include = [
                  ./src
                  ./lib
                  (nix-filter.lib.matchExt "cabal")
                ];
              };
          in haskellPackages.callCabal2nix packageName src { };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = haskellPackages.shellFor {
          packages = p: [ self.defaultPackage.${system} ];
          buildInputs = with haskellPackages;
            [
              ghc
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
