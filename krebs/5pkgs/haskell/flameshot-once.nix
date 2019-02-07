{ mkDerivation, async, base, blessings, dbus, fetchgit
, iso8601-time, process, stdenv, text, time, unagi-chan, unix
}:
mkDerivation {
  pname = "flameshot-once";
  version = "1.0.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/flameshot-once";
    sha256 = "0fjk5pgjy7r0xz4i38qb85x1z4jp8bas2mmgznp7glidz362w390";
    rev = "fb5636483871fbafe9b286b377c339c8ddf8b4f8";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base blessings dbus iso8601-time process text time unagi-chan
    unix
  ];
  license = stdenv.lib.licenses.mit;
}
