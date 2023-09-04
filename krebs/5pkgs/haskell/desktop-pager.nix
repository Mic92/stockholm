{ mkDerivation, aeson, base, blessings, bytestring, containers
, data-default, extra, fetchgit, hack, lib, optparse-applicative
, probability, scanner, speculate, split, terminal-size, text, unix
, utf8-string, X11
}:
mkDerivation {
  pname = "desktop-pager";
  version = "1.0.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/pager";
    sha256 = "07wjlhnb27vfhkqq5vhi768mlrcpwl4b2yfk04v3lw047q6pmby0";
    rev = "dfa3ff346d22d332ffbadd46963f1cc5cb2a4939";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base extra utf8-string X11 ];
  executableHaskellDepends = [
    aeson base blessings bytestring containers data-default hack
    optparse-applicative probability scanner speculate split
    terminal-size text unix X11
  ];
  license = lib.licenses.mit;
}
