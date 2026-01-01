{ mkElmDerivation }:
mkElmDerivation {
  name = "elm-docs-test";
  src = ../basic;
  buildPhase = "true";
  installPhase =
    ''
    mkdir $out
    cp -r .elm/0.19.1/packages/elm/core/1.0.5/docs.json $out
    '';
}
