{ mkDerivation, aeson, base, fetchgit, lib, X11 }:
mkDerivation {
  pname = "X11-aeson";
  version = "1.0.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/X11-aeson";
    sha256 = "0y9nvssqpvqgl46g7nz9738l8jmpa7an8r3am3qaqcvmvzgwxh0d";
    rev = "c0a70a62513baf2b437db4ebe3e5a32e3cfa5905";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ aeson base X11 ];
  license = lib.licenses.mit;
}
