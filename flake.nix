{
  description = "A flake giving mkElmDerivation and a programme to fetch and hash Elm Packages.";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskellPackages;
      in
        {
          overlay = final: prev: { mkElmDerivation = import ./mkElmDerivation.nix { inherit self system; } final prev;
                                   buildRegistryPackages = self.packages; };

          packages = {
            default = self.packages.${system}.elmHasher;
            elmHasher = import ./src/elmHasher/default.nix (haskellPackages // { lib = pkgs.lib; });
            snapshot = import ./src/snapshot/default.nix (haskellPackages // { lib = pkgs.lib; });

            # These require IFD. Use these for development, but not
            # for release. Run cabal2nix manually to update the
            # default.nix files. One needs to add ellipses to the
            # input attribute set in both these files.
            # elmHasher = haskellPackages.callCabal2nix "elmHasher" ./src/elmHasher { };
            # snapshot = haskellPackages.callCabal2nix "snapshot" ./src/snapshot { };
          };

          defaultPackage = self.packages.${system}.default;

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
