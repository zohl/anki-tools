{ mkDerivation, aeson, base, bytestring, data-default, directory
, exceptions, filepath, mtl, scientific, sqlite-simple, stdenv
, text, unordered-containers
}:
mkDerivation {
  pname = "anki-tools";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring data-default directory exceptions filepath
    mtl scientific sqlite-simple text unordered-containers
  ];
  executableHaskellDepends = [ base data-default ];
  description = "Tools for interacting with Anki database";
  license = stdenv.lib.licenses.bsd3;
}
