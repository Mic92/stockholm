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
    sha256 = "1qlkhqidaa6w02ix9ambfdsm7lfyx30ap481b9ic1ppyfkhqzfp6";
    rev = "fc6105a5e7d1e3a07bf07ea85e7902dd8e9fc849";
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
