{
  description = "My Haskell project";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
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
            (name:
              let len = builtins.stringLength name;
              in len >= 6 && (substring (len - 6) 6 name == ".cabal"))
            (attrNames (readDir ./.)));
          in head (match "^.*name\:\ *([^[:space:]]*).*$" (readFile "${./.}\/${cabalFileName}"));
      in
      {
        packages.${packageName} = haskellPackages.callCabal2nix packageName self { };

        packages.test = pkgs.stdenv.mkDerivation {
          name = "test";
          src = pkgs.fetchurl {
            url = "https://github.com/0ui/elm-task-parallel/archive/2.0.0.tar.gz";
            sha256 = with builtins; (fromJSON (readFile ./elmData.json))."0ui/elm-task-parallel"."2.0.0";
          };
          installPhase = "mkdir $out/; cat ${self.outputs.elmSrcsFunc ./elm.json} $out";

        };

        defaultPackage = self.packages.${system}.${packageName};

        devShell = haskellPackages.shellFor {
          packages = p: [ self.defaultPackage.${system} ]; # This automatically pulls cabal libraries into the devshell, so they can be used in ghci
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
