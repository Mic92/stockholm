{ mkDerivation, base, blessings, containers, data-default, fetchgit
, lens, lib, mtl, old-locale, process, scanner, time, unix, zippers
}:
mkDerivation {
  pname = "hack";
  version = "1.0.1";
  src = fetchgit {
    url = "https://cgit.krebsco.de/hack";
    sha256 = "0ry5ikn89ij512qvk1xhdhfz4s8a6b9yawgx6lxgnw5jkiyjd7ka";
    rev = "f3ea150aca5cc86878fa10bc5b1f0918fc154e2a";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  executableHaskellDepends = [
    base blessings containers data-default lens mtl old-locale process
    scanner time unix zippers
  ];
  license = lib.licenses.mit;
}
