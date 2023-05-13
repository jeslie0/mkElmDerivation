{ mkDerivation, aeson, base, bytestring, conduit, conduit-extra
, cryptonite, directory, hashable, http-conduit, http-types, lib
, memory, mtl, text, threads-pool, transformers
, unordered-containers, vector, ...
}:
mkDerivation {
  pname = "elmHasher";
  version = "0.2.0.1";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring conduit conduit-extra cryptonite directory
    hashable http-conduit http-types memory mtl text threads-pool
    transformers unordered-containers vector
  ];
  license = lib.licenses.mit;
  mainProgram = "ElmHasher";
}
