{ mkDerivation, async, base, blessings, bytestring, dbus, fetchgit
, iso8601-time, process, random, stdenv, text, time, unagi-chan
, unix
}:
mkDerivation {
  pname = "flameshot-once";
  version = "1.1.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/flameshot-once";
    sha256 = "158ha1yyj3p3mdjjga62j91ml83nhrsg34xbg3dir5cb399j8pxx";
    rev = "9d688b6ffad14912bd1afe42555747cb3d213d95";
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
