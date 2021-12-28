{ lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "ircaids";
  version = "1.1.0";

  src = pkgs.fetchgit {
    url = "https://cgit.krebsco.de/ircaids";
    rev = "refs/tags/${version}";
    sha256 = "05zd5dhsif00q8s1g5vzjd6x9n0c806nhfbdcgjdfgfy1j3kygyq";
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
