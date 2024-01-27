{ mkElmDerivation, elm }:
mkElmDerivation {
  name = "elm-example";
  src = ./.;
  nativeBuildInputs =
    [ elm ];
  buildPhase =
    ''
    elm make src/Main.elm --output Main.js --optimize
    '';
  installPhase =
    ''
    mkdir $out
    cp Main.js $out
    '';
}
