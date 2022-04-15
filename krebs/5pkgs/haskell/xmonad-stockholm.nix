{ mkDerivation, base, containers, directory, fetchgit, filepath
, lib, unix, X11, X11-xft, X11-xshape, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-stockholm";
  version = "1.3.1";
  src = fetchgit {
    url = "https://cgit.krebsco.de/xmonad-stockholm";
    sha256 = "1m4kkppy143jvjzhy5aawh8q6sglpnqhiajxbdcr42j02ibf3vvq";
    rev = "89bae8aad73db8fe9e11da7d515f0b236e7fea51";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base containers directory filepath unix X11 X11-xft X11-xshape
    xmonad xmonad-contrib
  ];
  license = lib.licenses.mit;
}
