{ mkElmDerivation }:
mkElmDerivation {
  name = "elm-example";
  src = ./.;
  buildPhase = "true";
  installPhase =
    ''
    mkdir $out
    cp -r .elm $out
    '';
}
