elmSrcsFunc:
with builtins;
{ pname ? (fromJSON (readFile elmJson))."name"
, version ? (fromJSON (readFile elmJson))."version"
, name ? pname + "-" + version
, nixpkgs
, src
, elmJson ? "${src}/elm.json"
, elmPackages ? elmSrcsFunc elmJson
, srcdir ?  "${src}/${head ((fromJSON (readFile elmJson))."source-directories")}"
, elmVersion ? (fromJSON (readFile elmJson))."elm-version"
, targets ? ["Main"]
, registryDat ? ./registry.dat
, outputJavaScript ? false
}:

with nixpkgs;
stdenv.mkDerivation {
  inherit pname version src;

  buildInputs = [ nixpkgs.elmPackages.elm ]
                ++ lib.optional outputJavaScript nodePackages.uglify-js;

  buildPhase = pkgs.elmPackages.fetchElmDeps {
    inherit elmPackages elmVersion registryDat;
  };

  installPhase =
    let elmfile = module: "${srcdir}/${builtins.replaceStrings ["."] ["/"] module}.elm";
        extension = if outputJavaScript then "js" else "html";
    in ''
       mkdir -p $out/share/doc
       ${lib.concatStrings (map (module: ''
         echo "compiling ${elmfile module}"
         elm make ${elmfile module} --output $out/${module}.${extension} --docs $out/share/doc/${module}.json
         ${lib.optionalString outputJavaScript ''
          echo "minifying ${elmfile module}"
          uglifyjs $out/${module}.${extension} --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
              | uglifyjs --mangle --output $out/${module}.min.${extension}
      ''}
    '') targets)}
  '';
}
