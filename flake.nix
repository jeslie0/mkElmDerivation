{
  description = "A flake containing useful tools for building Elm applications with Nix.";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    flake-utils = {
      url = github:numtide/flake-utils;
    };
    elm-spa = {
      url = github:jeslie0/elm-spa;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    elm-watch = {
      url = github:jeslie0/elm-watch;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, elm-spa, elm-watch }:
    let
      allPackagesJsonPath = ./mkElmDerivation/all-packages.json;
      elmHashesJsonPath = ./mkElmDerivation/elm-hashes.json;
      mkSnapshot = hPkgs: lib: import ./src/snapshot/default.nix (hPkgs // { inherit lib; });
      homepage = "https://github.com/jeslie0/mkElmDerivation";
      changelog = "https://github.com/jeslie0/mkElmDerivation/blob/main/CHANGELOG.org";
    in
    {
      overlay = builtins.trace "\"mkElmDerivation.overlay\" has been deprecated. Please use \"mkElmDerivation.overlays.mkElmDerivation\" instead." self.overlays.mkElmDerivation;

      overlays = {
        # The default overlay is the union of the other overlays in
        # this flake.
        default = final: prev:
          prev.lib.composeManyExtensions
            (builtins.attrValues (builtins.removeAttrs self.overlays ["default"])) final prev;

        mkElmDerivation = final: prev: {
          mkElmDerivation = with prev;
            import ./nix/mkElmDerivation.nix {
              inherit stdenv lib allPackagesJsonPath elmHashesJsonPath;
              elm = elmPackages.elm;
              uglify-js = nodePackages.uglify-js;
              snapshot = mkSnapshot haskellPackages lib;
            };
        };

        mkElmSpaDerivation = final: prev: {
          mkElmSpaDerivation = with prev;
            import ./nix/mkElmSpaDerivation.nix {
              inherit stdenv lib allPackagesJsonPath elmHashesJsonPath;
              elm = elmPackages.elm;
              elm-spa = elm-spa.packages.${system}.elmSpa;
              snapshot = mkSnapshot haskellPackages lib;
            };
        };

        mkElmWatchDerivation = final: prev: {
          mkElmWatchDerivation = with prev;
            import ./nix/mkElmWatchDerivation.nix {
              inherit allPackagesJsonPath elmHashesJsonPath lib stdenv;
              elm-watch = elm-watch.packages.${system}.elm-watch;
              snapshot = mkSnapshot haskellPackages lib;
            };
        };

        mkCEADerivation = final: prev: {
          mkCEADerivation = with prev;
            import ./nix/mkCEADerivation.nix {
              inherit allPackagesJsonPath elmHashesJsonPath lib stdenv;
              elm = elmPackages.elm;
              create-elm-app = elmPackages.create-elm-app;
              snapshot = mkSnapshot haskellPackages lib;
            };
        };

        mkDotElmDirectoryCmd = final: prev: {
          mkDotElmDirectoryCmd = with prev; (import ./nix/lib.nix {
            inherit allPackagesJsonPath lib stdenv;
            snapshot = mkSnapshot haskellPackages lib;
          }).mkDotElmCommand ./mkElmDerivation/elm-hashes.json;
        };
      };
    }
    // flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
      in
      {
        packages = {
          default = self.packages.${system}.elmHasher;
          elmHasher = (import ./src/elmHasher/default.nix (haskellPackages // { lib = pkgs.lib; })) // {
            meta = {
              description = "A program to fetch and hash all elm packages";
              homepage = homepage;
              changelog = changelog;
              license = pkgs.lib.licenses.mit;
            };
          };
          snapshot = (import ./src/snapshot/default.nix (haskellPackages // { lib = pkgs.lib; })) // {
            meta = {
              description = "A program to serialise an elm-packages.json to binary";
              homepage = homepage;
              changelog = changelog;
              license = pkgs.lib.licenses.bsd3;
            };
          };
          elmHashes = pkgs.stdenvNoCC.mkDerivation {
            name = "elmHashes";
            src = ./mkElmDerivation;
            installPhase = "mkdir $out; cp elm-hashes.json $out";
            meta = {
              description = "A JSON of elm packages and their hashes";
              homepage = homepage;
              changelog = changelog;
              license = pkgs.lib.licenses.mit;
            };
          };

          # These require IFD. Use these for development, but not
          # for release. Run cabal2nix manually to update the
          # default.nix files. One needs to add ellipses to the
          # input attribute set in both these files.
          # elmHasher = haskellPackages.callCabal2nix "elmHasher" ./src/elmHasher { };
          # snapshot = haskellPackages.callCabal2nix "snapshot" ./src/snapshot { };
        };

        defaultPackage = builtins.trace "defaultPackage has been deprecated. Please use packages.default." self.packages.${system}.default;

        devShell = haskellPackages.shellFor {
          packages = p: [
            self.packages.${system}.elmHasher
            self.packages.${system}.snapshot
          ];
          nativeBuildInputs = with haskellPackages;
            [
              haskell-language-server
              cabal-install
            ];

          # Enables Hoogle for the builtin packages.
          withHoogle = true;
        };
      }
    );
}
