{ stdenv, fetchgit, autoconf, automake, bitlbee-dev, glib, libgcrypt, libtool, pkgconfig }:

stdenv.mkDerivation rec {
  name = "bitlbee-steam-1.3.1";

  src = fetchgit {
    url = "https://github.com/jgeboski/bitlbee-steam";
    rev = "439d777c7e8d06712ffc15c3e51d61799f4c0d0c";
    sha256 = "493924da1083a3b23073c595a9e1989a7ae09a196524ad66ca99c4d8ccc20d2a";
  };

  buildInputs = [
    autoconf
    automake
    bitlbee-dev
    glib
    libgcrypt
    libtool
    pkgconfig
  ];

  configurePhase = ''
    ./autogen.sh
  '';

  installPhase = ''
    mkdir -p $out
    cp steam/.libs/steam.la $out/
    cp steam/.libs/steam.so $out/
  '';
}
