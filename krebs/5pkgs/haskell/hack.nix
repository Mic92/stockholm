{ mkDerivation, base, blessings, containers, data-default, fetchgit
, lens, mtl, old-locale, process, scanner, stdenv, time, unix
, zippers
}:
mkDerivation {
  pname = "hack";
  version = "1.0.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/hack";
    sha256 = "0hi6frpnxbg3h6s7gd48ri57jc226qycy4rnhmpzpq195xf8y3pf";
    rev = "cb004b2e5f0fce6cea8d54e60558a1c1904dbe39";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [
    base blessings containers data-default lens mtl old-locale process
    scanner time unix zippers
  ];
  license = stdenv.lib.licenses.mit;
}
