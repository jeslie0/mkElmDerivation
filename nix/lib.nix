{ allPackagesJsonPath, lib, snapshot, stdenv }:
rec {
  # Given a JSON of elm package hashes, an elm package name, and
  # version, return the derivation for the fetched packages.
  fetchElmPkg = elmHashesJson: name: version:
    let
      srcs = with builtins; [
        (fetchurl {
          url = "https://github.com/${name}/archive/${version}.tar.gz";
          sha256 =
            (fromJSON (readFile elmHashesJson)).${name}.${version}.archiveHash;
        })
        (fetchurl {
          url =
            "https://package.elm-lang.org/packages/${name}/${version}/docs.json";
          sha256 = (fromJSON (readFile elmHashesJson)).${name}.${version}.docsHash;
        })
      ];
      pname = lib.replaceStrings [ "/" ] [ "-" ] name;
    in
    stdenv.mkDerivation {
      unformattedName = name;
      inherit pname srcs version;

      sourceRoot = ".";

      # Unpack both the archive and the docs.json
      unpackPhase = ''
        runHook preUnpack

        mkdir source
        cd source
        echo srcs is "$srcs"
        unpackFile $srcs
        ls -la .
        mv ./*/* .

        cp ${builtins.elemAt srcs 1} ./docs.json

        cd ..

        runHook postUnpack
      '';

      # skip any Makefiles, if present
      buildPhase = "true";

      installPhase = ''
        mkdir -p $out
        cp -r source/* $out
      '';
    };

  fetchElmPkgVersions = elmHashesJson: name: versions:
    builtins.map (version: fetchElmPkg elmHashesJson name version) (lib.lists.unique versions);

  # Given a JSON of elm packages hashes and an elm.json, generate the
  # command to create the .elm directory
  mkDotElmCommand = elmHashesJson: elmJson: makeDotElmCommand elmHashesJson { inherit elmJson; };

  # Given a JSON of elm packages hashes and an elm.json, generate the
  # command to create the .elm directory.
  #
  # Optionally accepts extraDeps, for use with apps that manage their
  # dependencies outside elm.json; like elm-codegen and its elm.codegen.json file.
  # Pass a list of attr sets with the shape `[ { "miniBill/elm-unicode" = "1.0.2"; }; ]`.
  makeDotElmCommand = with builtins; elmHashesJson: { elmJson, extraDeps ? [] }:
    let
      dependencies =
        (fromJSON (readFile elmJson)).dependencies.direct //
        (fromJSON (readFile elmJson)).dependencies.indirect //
        (fromJSON (readFile elmJson)).test-dependencies.direct //
        (fromJSON (readFile elmJson)).test-dependencies.indirect;
      versionsPerPkg = lib.attrsets.zipAttrs ([dependencies] ++ extraDeps);
      derivationLists = lib.mapAttrsToList (fetchElmPkgVersions elmHashesJson) versionsPerPkg;
      elmVersion = (fromJSON (readFile elmJson))."elm-version";
      commandsList = builtins.map
        (pkg: ''
          mkdir -p .elm/${elmVersion}/packages/${pkg.unformattedName};
          cp -R ${pkg} .elm/${elmVersion}/packages/${pkg.unformattedName}/${pkg.version};
        '')
        (lib.lists.flatten derivationLists);
    in
    (
      lib.concatStrings commandsList) + ''
      export ELM_HOME=`pwd`/.elm
      mkdir -p .elm/${elmVersion}/packages;
      cp ${allPackagesJsonPath} ./all-packages.json
      ${snapshot}/bin/Snapshot
      mv ./registry.dat .elm/${elmVersion}/packages/registry.dat;
      chmod -R +w .elm;
    '';
}
