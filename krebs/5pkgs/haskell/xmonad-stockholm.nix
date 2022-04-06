{ mkDerivation, base, containers, directory, fetchgit, filepath
, lib, unix, X11, X11-xft, X11-xshape, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-stockholm";
  version = "1.3.1";
  src = fetchgit {
    url = "https://cgit.krebsco.de/xmonad-stockholm";
    sha256 = "1x5fjjrdgswv7fjnnmwvv2zicdszmkrsh564m7za4y54zg57wsx2";
    rev = "3cd58b7d415d320e67516be952be8d30c020bd81";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base containers directory filepath unix X11 X11-xft X11-xshape
    xmonad xmonad-contrib
  ];
  license = lib.licenses.mit;
}
