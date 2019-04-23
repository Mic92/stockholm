{ mkDerivation, async, base, blessings, bytestring, dbus, fetchgit
, iso8601-time, process, random, stdenv, text, time, unagi-chan
, unix
}:
mkDerivation {
  pname = "flameshot-once";
  version = "1.2.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/flameshot-once";
    sha256 = "01c11dk8ss37awfn9xqsgx668dcrf4kvzfxlq7ycnqsnpbjjvm0a";
    rev = "cebaefa37095e74ad2253c4e2f9d9ab390f88737";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base blessings bytestring dbus iso8601-time process random
    text time unagi-chan unix
  ];
  license = stdenv.lib.licenses.mit;
}
