{ mkDerivation, async, base, bytestring, fetchgit, network
, optparse-applicative, stdenv, text
}:
mkDerivation {
  pname = "kirk";
  version = "1.0.0";
  src = fetchgit {
    url = "http://cgit.krebsco.de/kirk";
    sha256 = "0w4drg2lyyw45abfn3g55zd6m7pl7yqxql5rpyy6qqdbvnyak94w";
    rev = "c78f3c62c0ba76465e39d1570073f867aa2d4240";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base bytestring network optparse-applicative text
  ];
  license = stdenv.lib.licenses.mit;
}
