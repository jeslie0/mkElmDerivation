{ mkElmDerivation }:
mkElmDerivation {
  name = "basic-test";
  src = ./.;
  outputJavaScript = true;
}
