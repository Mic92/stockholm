{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "scanner";
  version = "1.0.0";
  src = fetchgit {
    url = http://cgit.cd.retiolum/scanner;
    rev = "7f091a3bc152ad3974a1873b460fa1759bf8dcad";
    sha256 = "7d123c227777932039d26fc832b8d32a90f04c0bd6b7e8bcff0a6f49a54e0054";
  };
  libraryHaskellDepends = [ base ];
  license = stdenv.lib.licenses.wtfpl;
}
