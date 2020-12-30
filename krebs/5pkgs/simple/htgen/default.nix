{ fetchgit, lib, pkgs, stdenv }:
stdenv.mkDerivation rec {
  pname = "htgen";
  version = "1.3.0";

  src = fetchgit {
    url = "http://cgit.krebsco.de/htgen";
    rev = "refs/tags/v${version}";
    sha256 = "0p3517wkfpvip4z0axh0b4v1jm1nqpppldnhq4806c0p33vrjxnf";
  };

  installPhase = ''
    mkdir -p $out/bin
    {
      echo '#! ${pkgs.dash}/bin/dash'
      echo 'export PATH=${lib.makeBinPath [
        pkgs.coreutils
        pkgs.jq
        pkgs.ucspi-tcp
      ]}''${PATH+":$PATH"}'
      sed 's:^Server=htgen$:&/${version}:' htgen
    } > $out/bin/htgen
    chmod +x $out/bin/htgen
    cp -r examples $out
  '';
}
