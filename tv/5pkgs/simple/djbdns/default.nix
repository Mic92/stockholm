{ coreutils, gawk, fetchurl, stdenv, ... }:

with stdenv.lib;

stdenv.mkDerivation rec {
  name = "djbdns-1.05";
  src = fetchurl {
    url = "http://cr.yp.to/djbdns/djbdns-1.05.tar.gz";
    sha256 = "0j3baf92vkczr5fxww7rp1b7gmczxmmgrqc8w2dy7kgk09m85k9w";
  };
  configurePhase = ''
    echo $out > conf-home
    echo gcc -O2 -include errno.h > conf-cc
  '';
  patchPhase = ''
    sed -i 's:c("/","etc","dnsroots.global",-1,-1,0644);:// &:' hier.c
    sed -i '1s@^@PATH=${makeBinPath [ coreutils gawk ]}\n@' dnstracesort.sh
  '';
  installTargets = "setup check";
}
