{ stdenv, fetchgit, bison, flex, openssl }:

stdenv.mkDerivation rec {
  name = "charybdis-3.5.0-rc1";

  src = fetchgit {
    url = "https://github.com/atheme/charybdis.git";
    rev = "61815bf9324e872f51255e09fe37a8c595f94a60";
    sha256 = "1q9h7j2pm1wsbcybmm7r7hzimp1zda04lw9x2akb26l9p12zgfgc";
  };

  patches = [
    ./remove-setenv.patch
  ];

  configureFlags = [
    "--enable-epoll"
    "--enable-ipv6"
    "--enable-openssl=${openssl}"
    "--enable-small-net"
    "--with-program-prefix=charybdis-"
    "--sysconfdir=/tmp"
  ];

  buildInputs = [ bison flex openssl ];

  meta = {
    description = "An extremely scalable ircd with some cooperation with the ratbox and ircu guys";
    homepage    = https://github.com/atheme/charybdis;
    license     = stdenv.lib.licenses.gpl2;
    maintainers = [ stdenv.lib.maintainers.lassulus ];
    platforms   = stdenv.lib.platforms.linux;
  };
}
