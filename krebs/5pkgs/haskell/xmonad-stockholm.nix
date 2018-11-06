{ mkDerivation, base, containers, fetchgit, stdenv, X11, X11-xft, X11-xshape
, xmonad, xmonad-contrib
}:
mkDerivation rec {
  pname = "xmonad-stockholm";
  version = "1.2.0";
#  src = /home/jeschli/projects/haskell/xmonad-stockholm;
  src = fetchgit {
    url = http://cgit.ni.krebsco.de/xmonad-stockholm;
    rev = "refs/tags/v${version}";
    sha256 = "13mvmh3kk9a79l1nii028p0n7l95pb78wz9c4j42l90m02mg6cis";
  };
  libraryHaskellDepends = [
    base containers X11 X11-xft X11-xshape xmonad xmonad-contrib
  ];
  license = stdenv.lib.licenses.mit;
}
