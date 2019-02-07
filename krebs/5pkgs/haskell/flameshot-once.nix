{ mkDerivation, async, base, blessings, bytestring, dbus, fetchgit
, iso8601-time, process, random, stdenv, text, time, unagi-chan
, unix
}:
mkDerivation {
  pname = "flameshot-once";
  version = "1.0.1";
  src = fetchgit {
    url = "https://cgit.krebsco.de/flameshot-once";
    sha256 = "01bsgadjk3y3lg19xcadlrqalr4cs028fsivgacqh31fqaq4v243";
    rev = "03623ce6c011c1e85df7d91aed4458c098ff22ff";
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
