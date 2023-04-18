{ allPackagesJsonPath, elmHashesJsonPath, elm-watch, lib, snapshot, stdenv }:
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

  # Optional: The arguments passed to elm-watch make. Either
  # "--optimize", "--debug" or "". Defaults to "--optimize".
, option ? "--optimize"

  # Optional: The target names to build. Leave this empty to build all
  # targets. Defaults to [].
, targets ? [] # List of strings
, ...
} @ args:

stdenv.mkDerivation (args // {
  inherit pname version src;

  buildInputs = [ elm-watch ];

  buildPhase = (import ./lib.nix {
    inherit stdenv lib snapshot allPackagesJsonPath;
  }).mkDotElmCommand elmHashesJsonPath
    elmJson;

  installPhase =
    ''
      ${elm-watch}/bin/elm-watch make ${option} ${builtins.concatStringsSep " " targets}
      mkdir $out
      cp -r . $out
    '';
})
