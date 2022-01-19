{ mkDerivation, attoparsec, base, bytestring, containers, hashable
, lib, mtl, random, random-shuffle, unordered-containers
}:
mkDerivation {
  pname = "bayes";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base bytestring containers hashable mtl random
    random-shuffle unordered-containers
  ];
  executableHaskellDepends = [ base bytestring ];
  doHaddock = false;
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
