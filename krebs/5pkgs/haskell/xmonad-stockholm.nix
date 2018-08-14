{ mkDerivation, base, containers, fetchgit, stdenv, X11, X11-xft, X11-xshape
, xmonad, xmonad-contrib
}:
mkDerivation rec {
  pname = "xmonad-stockholm";
  version = "1.1.1";
  src = fetchgit {
    url = http://cgit.ni.krebsco.de/xmonad-stockholm;
    rev = "refs/tags/v${version}";
    sha256 = "05nnfg6q35z3qgf507qa80bz32jl4k719dl5phlmchplp3769585";
  };
  libraryHaskellDepends = [
    base containers X11 X11-xft X11-xshape xmonad xmonad-contrib
  ];
  license = stdenv.lib.licenses.mit;
}
