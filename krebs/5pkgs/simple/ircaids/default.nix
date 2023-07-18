{ lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "ircaids";
  version = "1.4.1";

  src = pkgs.fetchgit {
    url = "https://cgit.krebsco.de/ircaids";
    rev = "refs/tags/${version}";
    hash = "sha256-j16RLB3dIiynQqcbcK52MFk/2vStQBp1xkSwKItSYCM=";
  };

  buildPhase = null;

  installPhase = ''
    mkdir -p $out/bin

    cp $src/bin/ircsink $out/bin/ircsink
    sed -i '
      s;^#! /bin/sh;#! ${pkgs.dash}/bin/dash;
      s;^#!.*;&\nexport PATH=${lib.makeBinPath [
        pkgs.coreutils
        pkgs.gawk
        pkgs.gnused
        pkgs.netcat
        pkgs.nettools
        pkgs.openssl
        pkgs.unixtools.getopt
      ]};
    ' $out/bin/ircsink
  '';
}
