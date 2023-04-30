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

  # Optional: The project's elm-watch.json file. Will default to
  # ${src}/elm-watch.json.
, elmWatchJson ? "${src}/elm-watch.json"

  # Optional: The arguments passed to elm-watch make. Either
  # "--optimize", "--debug" or "". Defaults to "--optimize".
, option ? "--optimize"

  # Optional: The target names to build. Leave this empty to build all
  # relative targets. Defaults to [].
, targets ? [ ] # List of strings
, ...
} @ args:

let
  # Read the elm-watch.json file into an attribute set.
  elmWatchAttr = with builtins;
    (fromJSON (readFile elmWatchJson));

  # Extract all target names from elm-watch.json.
  allTargets = with builtins;
    attrNames elmWatchAttr.targets;

  # A predicate to check if a path is relative.
  isRelative = path: builtins.all (bool: bool == false) [
    (lib.hasPrefix "/" path)
    (lib.hasPrefix "." path)
    (lib.hasPrefix "~" path)
  ];

  # Extract all of the relative targets from elm-watch.json.
  allRelativeTargets = with builtins;
    filter (target: isRelative elmWatchAttr.targets.${target}.output) allTargets;

  # elm-watch matches its targets based on substrings, so we do the
  # same. This list is of all relative targets that have a substring
  # from targets.
  fixedTargets = with builtins;
    concatMap (target: filter (relTarget: lib.hasInfix target relTarget) allRelativeTargets) targets;

  # Given a target, create the command that creates directories and
  # copies it to $out.
  mkOutputCmd = target:
    let
      output = elmWatchAttr.targets.${target}.output;
      newDir = builtins.dirOf output;
    in
      ''
        mkdir -p $out/${newDir}
        cp ${output} $out/${newDir}
      '';

  # Build the command to extract targets to $out.
  targetExtractor = with builtins;
    concatStringsSep " "
      (if (targets == [])
       then map mkOutputCmd allRelativeTargets
       else map mkOutputCmd fixedTargets);

in

stdenv.mkDerivation (args // {
  inherit pname version src;

  buildInputs = [ elm-watch ];

  buildPhase = (import ./lib.nix {
    inherit stdenv lib snapshot allPackagesJsonPath;
  }).mkDotElmCommand elmHashesJsonPath
    elmJson;

  installPhase =
    ''
      runHook preInstall
      ${elm-watch}/bin/elm-watch make ${option} ${builtins.concatStringsSep " " targets}
      ${targetExtractor}
      runHook postInstall
    '';
})
