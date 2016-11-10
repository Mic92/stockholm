{ mkDerivation, base, containers, fetchgit, stdenv, X11, X11-xshape
, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-stockholm";
  version = "1.1.0";
  src = fetchgit {
    url = http://cgit.ni.krebsco.de/xmonad-stockholm;
    rev = "179d29fd4c765dee698058ef63295331ac603639";
    sha256 = "0c6mj68xsxxr4j8adkzhjszi7bg6cpisrsmqn587a16sblpbrnkj";
  };
  libraryHaskellDepends = [
    base containers X11 X11-xshape xmonad xmonad-contrib
  ];
  license = stdenv.lib.licenses.mit;
}
