{ stdenv, elm, lib, uglify-js, snapshot, allPackagesJsonPath, elmHashesJsonPath }:
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

  # Optional: The directory containing the .elm files to
  # compile. Currently, we only support using one directory.
, srcdir ? with builtins; "${src}/${head ((fromJSON (readFile elmJson))."source-directories")}"

  # Optional: The version of elm used. Read from elm.json file.
, elmVersion ? with builtins; (fromJSON (readFile elmJson))."elm-version"

  # Optional: The files in srcdir to compile.
, targets ? [ "Main" ]

  # Optional: Should the outputted files be JavaScript or HTML?
, outputJavaScript ? false
, ...
} @ args:

stdenv.mkDerivation (args // {
  inherit pname version src;

  buildInputs = [ elm ]
    ++ lib.optional outputJavaScript uglify-js;

  buildPhase = (import ./lib.nix {
    inherit stdenv lib snapshot allPackagesJsonPath;
  }).mkDotElmCommand elmHashesJsonPath
    elmJson;

  installPhase =
    let
      elmfile = module: "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
      extension = if outputJavaScript then "js" else "html";
    in
    ''
         mkdir -p $out/share/doc
         ${lib.concatStrings (map (module: ''
           echo "compiling ${elmfile module}"
           elm make ${elmfile module} --optimize --output $out/${module}.${extension} --docs $out/share/doc/${module}.json
           ${lib.optionalString outputJavaScript ''
            echo "minifying ${elmfile module}"
            uglifyjs $out/${module}.${extension}  --compress 'pure_funcs=[F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9],pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
            | uglifyjs --mangle --output $out/${module}.min.${extension}
        ''}
      '') targets)}
    '';
})
