{ mkDerivation, async, base, blessings, bytestring, dbus, fetchgit
, iso8601-time, process, random, stdenv, text, time, unagi-chan
, unix
}:
mkDerivation {
  pname = "flameshot-once";
  version = "1.3.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/flameshot-once";
    sha256 = "1jy73379srnkq79i7k3al406r0kb3pxwgg6f64i89jhzxjn7zmzl";
    rev = "81ce6b9bb68c2739ec5bda067fcfaeab931d55dd";
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
