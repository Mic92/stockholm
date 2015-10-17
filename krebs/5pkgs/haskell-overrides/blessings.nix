{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "blessings";
  version = "1.0.0";
  src = fetchgit {
    url = http://cgit.cd.retiolum/blessings;
    rev = "25a510dcb38ea9158e9969d56eb66cb1b860ab5f";
    sha256 = "b962153e80e51519b52220199d8350b54154833e4bc25a792ecc58898fef3fb2";
  };
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  # WTFPL is the true license, which is unknown to cabal.
  license = stdenv.lib.licenses.wtfpl;
}
