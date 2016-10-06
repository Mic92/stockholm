{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "scanner";
  version = "1.0.0";
  src = fetchgit {
    url = http://cgit.cd.krebsco.de/scanner;
    rev = "7f091a3bc152ad3974a1873b460fa1759bf8dcad";
    sha256 = "1lgl158axczsm4fx53fyq1d4116v91jsx4dbz66ka4k1ljqrmhgn";
  };
  libraryHaskellDepends = [ base ];
  license = stdenv.lib.licenses.wtfpl;
}
