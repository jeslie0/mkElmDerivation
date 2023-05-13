{ mkDerivation, aeson, base, binary, bytestring, containers
, directory, lib, mtl, text, ...
}:
mkDerivation {
  pname = "Snapshot";
  version = "0.1.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base binary bytestring containers directory mtl text
  ];
  license = lib.licenses.bsd3;
  mainProgram = "Snapshot";
}
