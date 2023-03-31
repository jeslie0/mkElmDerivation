{
  description = "A flake containing useful tools for building Elm applications with Nix.";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
    elm-spa = {
      url = github:jeslie0/elm-spa;
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs = { self, nixpkgs, flake-utils, elm-spa }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
      in
        {
          overlay = builtins.trace "\"mkElmDerivation.overlay.${system}.overlay\" has been deprecated. Please use \"mkElmDerivation.overlays.${system}.default\" instead." self.overlays.${system}.default;

          overlays = {
            default = self.overlays.${system}.mkElmDerivation;
            mkElmDerivation = final: prev: {
              mkElmDerivation = import ./mkElmDerivation.nix { inherit self system; } final prev;
              buildRegistryPackages = self.packages;
            };
            mkElmSpaDerivation = final: prev: {
              mkElmSpaDerivation = import ./mkElmSpaDerivation.nix { inherit self system; } final prev;
              elmPackages.elm-spa = elm-spa.packages.${system}.default;
              buildRegistryPackages = self.packages;
            };
          };

          packages = {
            default = self.packages.${system}.elmHasher;
            elmHasher = import ./src/elmHasher/default.nix (haskellPackages // { lib = pkgs.lib; });
            snapshot = import ./src/snapshot/default.nix (haskellPackages // { lib = pkgs.lib; });
            elmHashes = pkgs.stdenvNoCC.mkDerivation {
              name = "elmHashes";
              src = ./mkElmDerivation;
              installPhase = "mkdir $out; cp elm-hashes.json $out";
              meta.description = "A JSON of elm packages and their hashes.";
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
            packages = p: [ self.packages.${system}.elmHasher
                            self.packages.${system}.snapshot ];
            buildInputs = with haskellPackages;
              [ haskell-language-server
                cabal-install ];

            # Enables Hoogle for the builtin packages.
            withHoogle = true;
          };
        }
    );
}
