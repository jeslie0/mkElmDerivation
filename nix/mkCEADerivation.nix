{ allPackagesJsonPath, elm, elmHashesJsonPath, create-elm-app, lib, snapshot, stdenv }:
{
  # The name of the elm project.
  pname

  # The version of the elm project.
, version

  # The base directory of your elm project (most likely ./.).
, src

  # Optional: The elm project's elm.json file. Will default to
  # ${src}/elm.json
, elmJson ? "${src}/elm.json"
, ...
} @ args:

stdenv.mkDerivation (args // {
  inherit pname version src;

  buildInputs = [ elm create-elm-app ];

  buildPhase = (import ./lib.nix {
    inherit stdenv lib snapshot allPackagesJsonPath;
  }).mkDotElmCommand elmHashesJsonPath
    elmJson;

  installPhase =
    ''
      runHook preInstall
      ${create-elm-app}/bin/elm-app build
      mkdir $out
      cp -r build/* $out
      runHook postInstall
    '';
})
