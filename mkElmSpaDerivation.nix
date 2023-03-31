# This file is an overlay
{self, system}: final: prev:
{ # The name of the elm project.
  pname

  # The version of the elm project.
, version

  # The base directory of your elm project (most likely ./.).
, src

  # Optional: The elm projects elm.json file. Will default to
  # ${src}/elm.json
, elmJson ? "${src}/elm.json"
}:

prev.stdenv.mkDerivation {
  inherit pname version src;

  buildInputs = [ prev.elmPackages.elm
                  final.elmPackages.elm-spa
                ];

  buildPhase = (import ./mkElmDerivation/lib.nix { inherit self system; } final prev).mkDotElmCommand ./mkElmDerivation/elm-hashes.json elmJson;

  installPhase =
    ''
    ${final.elmPackages.elm-spa}/bin/elm-spa build
    mkdir $out
    cp -r public/* $out
    '';
}
