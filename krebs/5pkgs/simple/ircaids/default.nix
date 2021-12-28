{ lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "ircaids";
  version = "1.2.0";

  src = pkgs.fetchgit {
    url = "https://cgit.krebsco.de/ircaids";
    rev = "refs/tags/${version}";
    sha256 = "049ln54llfrn99q0pzwlb7iaz4igd4f3n4rb6mpc9irsy32bv3qg";
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
