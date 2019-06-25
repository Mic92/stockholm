{ mkDerivation, base, containers, fetchgit, filepath, stdenv, unix, X11, X11-xft , X11-xshape, xmonad, xmonad-contrib
}:
mkDerivation rec {
  pname = "xmonad-stockholm";
  version = "1.3.0";
  src = fetchgit {
    url = http://cgit.ni.krebsco.de/xmonad-stockholm;
    rev = "refs/tags/v1.3.0";
    sha256 = "1np5126wn67y0a1r60rnkq828s0w9zjnvai4b8zy3yc02xlkrjm9";
  };
  libraryHaskellDepends = [
    base containers filepath unix X11 X11-xft X11-xshape xmonad xmonad-contrib
  ];
  license = stdenv.lib.licenses.mit;
}
