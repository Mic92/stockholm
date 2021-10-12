{ lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "ircaids";
  version = "1.0.0";

  src = pkgs.fetchgit {
    url = "https://cgit.krebsco.de/ircaids";
    rev = "refs/tags/${version}";
    sha256 = "13z9pc9vq2xq2qpavwmh7fvzvvjkc495ssxsh8cs044qflvj54b2";
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
        pkgs.utillinux
      ]};
    ' $out/bin/ircsink
  '';
}
