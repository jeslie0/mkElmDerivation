{ mkElmDerivation, elm-optimize-level-2, elm }:
mkElmDerivation {
  name = "elm-optimize-level-2-test";
  src = ./.;
  outputJavaScript = true;
  nativeBuildInputs = [ elm-optimize-level-2 elm ];
  buildPhase = "elm-optimize-level-2 --optimize-speed src/Main.elm";
  installPhase = "mkdir $out; cp -r * $out";
}
