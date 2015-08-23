{ fetchurl, stdenv, gnutls, glib, pkgconfig, check, libotr, python }:

stdenv.mkDerivation rec {
  name = "bitlbee-3.4.1";

  src = fetchurl {
    url = "mirror://bitlbee/src/${name}.tar.gz";
    sha256 = "1qf0ypa9ba5jvsnpg9slmaran16hcc5fnfzbb1sdch1hjhchn2jh";
  };

  buildInputs = [ gnutls glib pkgconfig libotr python ];

  buildPhase = "";

  installPhase = ''
    make install-dev
  '';

}

