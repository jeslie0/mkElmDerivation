{self, system}:
final:
prev:
rec {
  # Given a JSON of elm package hashes, an elm package name, and
  # version, return the derivation for the fetched packages.
  fetchElmPkg = elmHashesJson: name: version:
prev.stdenv.mkDerivation {
    unformattedName = name;
    pname =
prev.lib.replaceStrings [ "/" ] [ "-" ] name;
    version = version;
    src = with builtins; fetchurl {
      url = "https://github.com/${name}/archive/${version}.tar.gz";
      sha256 = (fromJSON (readFile elmHashesJson)).${name}.${version};
    };
    installPhase = ''
      mkdir -p $out
      cp -r * $out
    '';
  };

  # Given a JSON of elm packages hashes and an elm.json, generate the
  # command to create the .elm directory
  mkDotElmCommand = with builtins; elmHashesJson: elmJson:
    let
      dependencies =
        (fromJSON (readFile elmJson)).dependencies.direct //
        (fromJSON (readFile elmJson)).dependencies.indirect //
        (fromJSON (readFile elmJson)).test-dependencies.direct //
        (fromJSON (readFile elmJson)).test-dependencies.indirect;
      derivationList =
prev.lib.mapAttrsToList (fetchElmPkg elmHashesJson) dependencies;
      elmVersion = (fromJSON (readFile elmJson))."elm-version";
      commandsList = builtins.map
        (pkg: ''
          mkdir -p .elm/${elmVersion}/packages/${pkg.unformattedName};
          cp -R ${pkg} .elm/${elmVersion}/packages/${pkg.unformattedName}/${pkg.version};
        '')
        derivationList;
    in
    (
prev.lib.concatStrings commandsList) + ''
      export ELM_HOME=`pwd`/.elm
      mkdir -p .elm/${elmVersion}/packages;
      cp ${self}/mkElmDerivation/all-packages.json ./all-packages.json
      ${final.buildRegistryPackages.${system}.snapshot}/bin/Snapshot
      mv ./registry.dat .elm/${elmVersion}/packages/registry.dat;
      chmod -R +w .elm;
    '';



}