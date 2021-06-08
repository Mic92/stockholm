{ mkDerivation, async, base, blessings, bytestring, dbus, fetchgit
, iso8601-time, lib, process, random, text, time, unagi-chan, unix
}:
mkDerivation {
  pname = "flameshot-once";
  version = "1.4.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/flameshot-once";
    sha256 = "13szgsiwn29aixm5xvs1m7128y5km5xss0ry5ii5y068rc2vysw8";
    rev = "4475893c2081b3d9db4b7a54d0ce38d0914a17bf";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base blessings bytestring dbus iso8601-time process random
    text time unagi-chan unix
  ];
  license = lib.licenses.mit;
}
