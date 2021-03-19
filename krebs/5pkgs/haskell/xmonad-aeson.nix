{ mkDerivation, aeson, base, fetchgit, stdenv, X11-aeson, xmonad }:
mkDerivation {
  pname = "xmonad-aeson";
  version = "1.0.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/xmonad-aeson";
    sha256 = "0l1gna6p1498vzm6kj0ywj0i7775mz5n7k9nymwggvfb1pyxv3h9";
    rev = "a95f652b150f17db3f2439214a6346335d6d8d89";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [ aeson base X11-aeson xmonad ];
  license = stdenv.lib.licenses.mit;
}
