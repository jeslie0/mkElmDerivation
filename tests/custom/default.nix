{ mkElmDerivation }:
mkElmDerivation {
  name = "custom-test";
  src = ./.;
  outputJavaScript = true;
  buildPhase = "elm make src/Main.elm";
  installPhase = "mkdir $out; cp -r * $out";
}
