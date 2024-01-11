{ stdenv, elm, lib, uglify-js, snapshot, allPackagesJsonPath, elmHashesJsonPath }:
{
  # This set is passed into a mkDerivation call. It needs a pname and
  # version, or just a name.


  # The base directory of your elm project (most likely ./.).
  src ? ./.

  # Optional: The elm project's elm.json file. Will default to
  # ${src}/elm.json
, elmJson ? "${src}/elm.json"

  # Optional: List of elm files to build, relative to src. Defaults to
  # "Main.elm" in the directories listed in the elm.json
  # source-directories value.
, targets ? with builtins;
  map (path: "${src}/" + path + "/Main.elm") ((fromJSON (readFile elmJson))."source-directories")

  # Optional: The version of elm used. Read from elm.json file.
, elmVersion ? with builtins; (fromJSON (readFile elmJson))."elm-version"

  # Optional: Should the outputted files be JavaScript or HTML?
, outputJavaScript ? false

  # Optional: Should the documentation JSON be generated?
, docs ? false

  # Optional: What level of optimization should be applied to the elm
  # code?
  # 0 - Level 0: No optimization,
  # 1 - Level 1: Elm's own optimization only (elm make --optimize),
  # 2 - Level 1 and uglifyjs's minification. This is the most
  # optimized and is only  available for JavaScript code.
, optimizationLevel ? 2

, ...
} @ args:
let
  # The output file extension.
  extension = if outputJavaScript then "js" else "html";

  # Command to make the docs directory
  makeDocsDirectory = if docs then "mkdir -p $out/share/doc " else " ";

  # Command to make the documentation JSON
  docsCommand = basename: if docs then "--docs $out/share/doc/${basename}.json " else " ";

  # elm's optimization flag
  optimizeCommand = if optimizationLevel == 0 then " " else "--optimize ";

  # The output field, added to the elm make command
  outputCommand = dotElmFreeBaseName: "--output $out/${dotElmFreeBaseName}.${extension}";

  # The elm make command with optimization added if requested.
  elmMakeCommand = target:
    let basename = builtins.baseNameOf target;
        dotElmFreeBasename = lib.removeSuffix ".elm" basename;
    in
    ''
      echo "compiling ${target}"
      elm make ${target} \
      ${outputCommand dotElmFreeBasename} \
      ${optimizeCommand} \
      ${docsCommand dotElmFreeBasename}
      ${uglifyjsCommand dotElmFreeBasename}
    '';

  # Command to minimize the elm output further.
  uglifyjsCommand = dotElmFreeBasename:
    if optimizationLevel == 2 && outputJavaScript
    then ''
      echo "optimizing ${dotElmFreeBasename}.${extension}"

      uglifyjs $out/${dotElmFreeBasename}.${extension} \
      --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
      | uglifyjs --mangle --output $out/${dotElmFreeBasename}.min.${extension}

      rm $out/${dotElmFreeBasename}.${extension}
    ''
    else " ";
in

stdenv.mkDerivation (args // {
  nativeBuildInputs =
    if builtins.hasAttr "nativeBuildInputs" args
    then args.nativeBuildInputs
    else
      [ elm ]
      ++ lib.optional outputJavaScript uglify-js;

  buildPhase =
    ''
    runHook preBuild
    ${(import ./lib.nix {inherit stdenv lib snapshot allPackagesJsonPath;}).mkDotElmCommand elmHashesJsonPath elmJson}
    ${if builtins.hasAttr "buildPhase" args
      then args.buildPhase
      else
        ''
        ${makeDocsDirectory}
        ${builtins.concatStringsSep "\n" (builtins.map elmMakeCommand targets)}
        ''}
    runHook postBuild
    '';

  installPhase =
    if builtins.hasAttr "installPhase" args
    then args.installPhase
    else
    ''
    runHook preInstall
    runHook postInstall
    '';
})
