{ mkDerivation, aeson, base, bytestring, containers, directory
, extra, filepath, lib, systemd, template-haskell, th-env
, transformers, unix, X11, xmonad, xmonad-contrib
}:
mkDerivation {
  pname = "xmonad-tv";
  version = "1.0.0";
  src = ./src;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base bytestring containers directory extra filepath systemd
    template-haskell th-env transformers unix X11 xmonad xmonad-contrib
  ];
  license = lib.licenses.mit;
}
