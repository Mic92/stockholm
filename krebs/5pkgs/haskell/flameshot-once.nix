{ mkDerivation, async, base, blessings, bytestring, dbus, fetchgit
, iso8601-time, lib, process, random, text, time, unagi-chan, unix
}:
mkDerivation {
  pname = "flameshot-once";
  version = "1.4.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/flameshot-once";
    sha256 = "03g6sxgp6hcmbww5lzbs5llssgii1w469i5pz14x94542l06cmkq";
    rev = "5f0ba1cf326d215bd5c50ad74c634e92c785ae46";
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
