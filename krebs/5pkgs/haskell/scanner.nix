{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "scanner";
  version = "1.0.1";
  src = fetchgit {
    url = "http://cgit.ni.krebsco.de/scanner";
    sha256 = "1lgl158axczsm4fx53fyq1d4116v91jsx4dbz66ka4k1ljqrmhgn";
    rev = "7f091a3bc152ad3974a1873b460fa1759bf8dcad";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ base ];
  license = stdenv.lib.licenses.mit;
}
