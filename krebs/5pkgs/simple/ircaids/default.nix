{ lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "ircaids";
  version = "1.3.0";

  src = pkgs.fetchgit {
    url = "https://cgit.krebsco.de/ircaids";
    rev = "refs/tags/${version}";
    sha256 = "128ryfl0prpc1789hhqw2mq16zy3jd82a24k6hkw7nj71hifzr3a";
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
