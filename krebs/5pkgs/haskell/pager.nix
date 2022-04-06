{ mkDerivation, base, blessings, bytestring, containers
, data-default, hack, lib, optparse-applicative, probability
, scanner, speculate, split, terminal-size, text, unix, X11
, fetchgit
}:
mkDerivation {
  pname = "pager";
  version = "1.0.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/pager";
    sha256 = "1kqd27faxinkwpxancyk0xl6n7ljlc8iqhnnq85l76bk4qi9b45i";
    rev = "f4cdf79bd4a75e9eafe68b9a908f4cc68682b7ef";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base blessings bytestring containers data-default hack
    optparse-applicative probability scanner speculate split
    terminal-size text unix X11
  ];
  license = lib.licenses.mit;
}
