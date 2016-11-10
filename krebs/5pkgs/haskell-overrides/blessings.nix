{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "blessings";
  version = "1.0.0";
  src = fetchgit {
    url = http://cgit.ni.krebsco.de/blessings;
    rev = "25a510dcb38ea9158e9969d56eb66cb1b860ab5f";
    sha256 = "0xg329h1y68ndg4w3m1jp38pkg3gqg7r19q70gqqj4mswb6qcrqc";
  };
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  # WTFPL is the true license, which is unknown to cabal.
  license = stdenv.lib.licenses.wtfpl;
}
