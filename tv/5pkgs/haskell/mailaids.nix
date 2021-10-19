{ mkDerivation, aeson, aeson-pretty, base, bytestring
, case-insensitive, fetchgit, lens, optparse-applicative
, purebred-email, lib, text, vector, word8
}:
mkDerivation {
  pname = "mailaids";
  version = "1.0.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/mailaids";
    sha256 = "15h0k82czm89gkwhp1rwdy77jz8dmb626qdz7c2narvz9j7169v5";
    rev = "8f11927ea74d6adb332c884502ebd9c486837523";
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
