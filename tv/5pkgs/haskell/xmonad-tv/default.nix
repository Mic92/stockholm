{ mkDerivation, base, containers, directory, extra, stdenv, unix
, X11, xmonad, xmonad-contrib, xmonad-stockholm
}:
mkDerivation {
  pname = "xmonad-tv";
  version = "1.0.0";
  src = ./src;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base containers directory extra unix X11 xmonad xmonad-contrib
    xmonad-stockholm
  ];
  license = stdenv.lib.licenses.mit;
}
