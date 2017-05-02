{ mkDerivation, async, base, bytestring, fetchgit, network
, optparse-applicative, stdenv, text
}:
mkDerivation {
  pname = "irc";
  version = "1.0.0";
  src = fetchgit {
    url = "http://cgit.krebsco.de/irc";
    sha256 = "174ywhvidybg49m4b43q2304izwbx3s7bvipk9g399zjyb392r8f";
    rev = "7225d47e9c1f4c7032ad55fbe1d9f33ff205549c";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base bytestring network optparse-applicative text
  ];
  license = stdenv.lib.licenses.mit;
}
