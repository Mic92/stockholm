{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation rec {
  pname = "blessings";
  version = "1.1.0";
  src = fetchgit {
    url = http://cgit.ni.krebsco.de/blessings;
    rev = "refs/tags/v${version}";
    sha256 = "1k908zap3694fcxdk4bb29s54b0lhdh557y10ybjskfwnym7szn1";
  };
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  # WTFPL is the true license, which is unknown to cabal.
  license = stdenv.lib.licenses.wtfpl;
}
