{
  description = "A flake giving mkElmDerivation and a programme to fetch and hash Elm Packages.";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }:
    {
      overlay = final: prev: { mkElmDerivation =  import ./mkElmDerivation.nix final prev; };
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

          mkElmDerivation = import ./mkElmDerivation.nix null pkgs;

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
