{ mkDerivation, aeson, base, bytestring, containers, directory
, extra, lib, template-haskell, th-env, unix, X11, xmonad
, xmonad-contrib, xmonad-stockholm
}:
mkDerivation {
  pname = "xmonad-tv";
  version = "1.0.0";
  src = ./src;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers directory extra template-haskell
    th-env unix X11 xmonad xmonad-contrib xmonad-stockholm
  ];
  license = lib.licenses.mit;
}
