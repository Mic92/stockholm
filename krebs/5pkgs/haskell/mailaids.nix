{ mkDerivation, aeson, aeson-pretty, base, bytestring
, case-insensitive, fetchgit, lens, lib, optparse-applicative
, purebred-email, text, vector, word8
}:
mkDerivation {
  pname = "mailaids";
  version = "1.1.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/mailaids";
    sha256 = "0mkq3b0j28h7ydg6aaqlqnvajb8nhdc9g7rmil2d4vl5fxxaqspv";
    rev = "a25fc32eceefc10a91ef77ff2763b3f1b9324aaf";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring case-insensitive lens
    optparse-applicative purebred-email text vector word8
  ];
  license = lib.licenses.mit;
}
