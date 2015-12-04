{ mkDerivation, base, containers, fetchgit, stdenv, X11, X11-xshape
, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-stockholm";
  version = "1.0.0";
  src = fetchgit {
    url = "http://cgit.cd.krebsco.de/xmonad-stockholm";
    sha256 = "35dda5d16acc90af94ae2fae10ab5cc2d5b450c3f1ff2e7f515ac53877269abf";
    rev = "2dbefe42fc5cfe9093465bf3e22ba8f82feeef6e";
  };
  libraryHaskellDepends = [
    base containers X11 X11-xshape xmonad xmonad-contrib
  ];
  license = stdenv.lib.licenses.mit;
}
