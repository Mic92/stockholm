{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation rec {
  pname = "blessings";
  version = "1.2.0";
  src = fetchgit {
    url = http://cgit.ni.krebsco.de/blessings;
    rev = "refs/tags/v${version}";
    sha256 = "03hz43ixww0h4fwxqrlrlvmj3pxswhb50ijaapwjz8457il2r300";
  };
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  # WTFPL is the true license, which is unknown to cabal.
  license = stdenv.lib.licenses.wtfpl;
}
