{ stdenv, fetchurl, cmake, doxygen, zlib, openssl, bzip2, pkgconfig, libpthreadstubs }:

stdenv.mkDerivation rec {
  name = "apt-cacher-ng-${version}";
  version = "0.9.3";

  src = fetchurl {
    url = "http://ftp.debian.org/debian/pool/main/a/apt-cacher-ng/apt-cacher-ng_${version}.orig.tar.xz";
    sha256 = "1bd7l1wg0q1p9pg0v6lqflf2znydx8mrh2jxbvv6xsrp1473nfwg";
  };

  NIX_LDFLAGS = "-lpthread";
  buildInputs = [ doxygen cmake zlib openssl bzip2 pkgconfig libpthreadstubs ];

  meta = {
    description = "A caching proxy specialized for linux distribution files";
    homepage = http://www.unix-ag.uni-kl.de/~bloch/acng/;
    license = stdenv.lib.licenses.gpl2;
    maintainers = [ stdenv.lib.maintainers.makefu ];
  };
}
