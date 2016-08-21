{ stdenv, fetchurl, cmake, doxygen, zlib, openssl, bzip2, pkgconfig, libpthreadstubs }:

stdenv.mkDerivation rec {
  name = "apt-cacher-ng-${version}";
  version = "0.9.3.2";

  src = fetchurl {
    url = "http://ftp.debian.org/debian/pool/main/a/apt-cacher-ng/apt-cacher-ng_${version}.orig.tar.xz";
    sha256 = "1bvng9mwrggvc93q2alj0x72i56wifnjs2dsycr17mapsv0f2gnc";
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
