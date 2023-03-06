{
  description = "A flake giving mkElmDerivation and a programme to fetch and hash Elm Packages.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    { elmSrcsFunc = with builtins;
        elmJson:
        let
          elmHashes = fromJSON (readFile ./mkElmDerivation/elm-hashes.json);
          getHash = name: ver: elmHashes.${name}.${ver};
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
          let haskellDir = ./src;
              cabalFileName = head (filter
                (name: pkgs.lib.hasSuffix ".cabal" name)
                (attrNames (readDir haskellDir)));
          in head (match "^.*name\:\ *([^[:space:]]*).*$" (readFile "${haskellDir}\/${cabalFileName}"));
      in
      {
        packages = {
          default = self.packages.${system}.${packageName};
          ${packageName} = haskellPackages.callCabal2nix packageName ./src { };
        };

        defaultPackage = self.packages.${system}.default;

        devShell = haskellPackages.shellFor {
          packages = p: [ self.packages.${system}.default ];
          buildInputs = with haskellPackages;
            [ haskell-language-server
              cabal-install
            ];

          # Enables Hoogle for the builtin packages.
          withHoogle = true;
        };
      }
    );
}
