{
  description = "A flake containing useful tools for building Elm applications with Nix.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    elm-spa = {
      url = "github:jeslie0/elm-spa";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    elm-watch = {
      url = "github:jeslie0/elm-watch";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, elm-spa, elm-watch }:
    let
      supportedSystems =
        [ "aarch64-linux" "x86_64-linux" "aarch64-darwin" "x86_64-darwin" ];

      forAllSystems =
        nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
        });

      allPackagesJsonPath =
        ./mkElmDerivation/all-packages.json;

      elmHashesJsonPath =
        ./mkElmDerivation/elm-hashes.json;

      snapshot =
        system: import ./src/snapshot/default.nix (nixpkgsFor.${system}.haskellPackages // { lib = nixpkgsFor.${system}.lib; });

      homepage =
        "https://github.com/jeslie0/mkElmDerivation";

      changelog =
        "https://github.com/jeslie0/mkElmDerivation/blob/main/CHANGELOG.org";

      mkElmDerivation = pkgs: with pkgs;
        import ./nix/mkElmDerivation.nix {
          inherit stdenv lib allPackagesJsonPath elmHashesJsonPath;
          elm = elmPackages.elm;
          uglify-js = nodePackages.uglify-js;
          snapshot = snapshot system;
        };
    in
    {
      overlay =
        builtins.trace "\"mkElmDerivation.overlay\" has been deprecated. Please use \"mkElmDerivation.overlays.mkElmDerivation\" instead." self.overlays.mkElmDerivation;

      overlays = {
        # The default overlay is the union of the other overlays in
        # this flake.
        default = final: prev:
          prev.lib.composeManyExtensions
            (builtins.attrValues
              (builtins.removeAttrs self.overlays ["default"])
            ) final prev;

        mkElmDerivation = final: prev: {
          mkElmDerivation = mkElmDerivation prev;
        };

        mkElmSpaDerivation = final: prev: {
          mkElmSpaDerivation = with prev;
            import ./nix/mkElmSpaDerivation.nix {
              inherit stdenv lib allPackagesJsonPath elmHashesJsonPath;
              elm = elmPackages.elm;
              elm-spa = elm-spa.packages.${system}.elmSpa;
              snapshot = snapshot prev.system;
            };
        };

        mkElmWatchDerivation = final: prev: {
          mkElmWatchDerivation = with prev;
            import ./nix/mkElmWatchDerivation.nix {
              inherit allPackagesJsonPath elmHashesJsonPath lib stdenv;
              elm-watch = elm-watch.packages.${system}.elm-watch;
              snapshot = snapshot prev.system;
            };
        };

        mkDotElmDirectoryCmd = final: prev: {
          mkDotElmDirectoryCmd = with prev; (import ./nix/lib.nix {
            inherit allPackagesJsonPath lib stdenv;
            snapshot = snapshot prev.system;
          }).mkDotElmCommand ./mkElmDerivation/elm-hashes.json;
        };
      };

      packages =
        forAllSystems
          (system:
            let
              pkgs =
                nixpkgsFor.${system};

              haskellPackages =
                pkgs.haskellPackages;
            in
              {
                default = self.packages.${system}.elmHasher;

                elmHasher =
                  (import ./src/elmHasher/default.nix (haskellPackages // { lib = pkgs.lib; }))
                  //
                  {
                    meta = {
                      description = "A program to fetch and hash all elm packages";
                      homepage = homepage;
                      changelog = changelog;
                      license = pkgs.lib.licenses.mit;
                    };
                  };

                elmHashes =
                  pkgs.stdenvNoCC.mkDerivation {
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
              }
          );

      # These require IFD. Use these for development, but not
      # for release. Run cabal2nix manually to update the
      # default.nix files. One needs to add ellipses to the
      # input attribute set in both these files.
      # elmHasher = haskellPackages.callCabal2nix "elmHasher" ./src/elmHasher { };
      # snapshot = haskellPackages.callCabal2nix "snapshot" ./src/snapshot { };

      defaultPackage =
        forAllSystems (system:
          builtins.trace "defaultPackage has been deprecated. Please use packages.default." self.packages.${system}.default
        );

      checks =
        forAllSystems (system:
          let
            pkgs =
              nixpkgsFor.${system};
          in
            {
              basic =
                import ./tests/basic/default.nix {
                  mkElmDerivation = mkElmDerivation pkgs;
                  elm = pkgs.elmPackages.elm;
                };

              custom =
                import ./tests/custom/default.nix {
                  mkElmDerivation = mkElmDerivation pkgs;
                };

              elm-optimize-level-2 =
                import ./tests/elm-level-2/default.nix {
                  mkElmDerivation = mkElmDerivation pkgs;
                  elm-optimize-level-2 = pkgs.elmPackages.elm-optimize-level-2;
                  elm = pkgs.elmPackages.elm;
                };
            }
        );

      devShell =
        forAllSystems (system:
          let
            pkgs =
              nixpkgsFor.${system};

            haskellPackages =
              pkgs.haskellPackages;
          in
            haskellPackages.shellFor {
              packages = p: [
                self.packages.${system}.elmHasher
              ];
              nativeBuildInputs = with haskellPackages;
                [
                  # haskell-language-server
                  cabal-install
                ];

              # Enables Hoogle for the builtin packages.
              withHoogle = true;
            }
        );
    };
}
