{ bash, coreutils, gnused, stdenv, fetchgit, script ? "", ucspi-tcp }:
with import <stockholm/lib>;
let
  version = "1.0";
in stdenv.mkDerivation {
  name = "htgen-${version}";

  src = fetchgit {
    url = "http://cgit.krebsco.de/htgen";
    rev = "refs/v1.0";
    sha256 = "15z451f57ddaxm21dlqqx2kavzyqx4sgnnzz4ql6vl237979g09s";
  };

  installPhase = ''
    find
    mkdir -p $out/bin
    {
      echo '#! ${bash}/bin/bash'
      echo 'export PATH=${makeBinPath [
        ucspi-tcp
        coreutils
        gnused
      ]}'
      sed -n '/^reply_404$/q;p' < htgen
      printf '%s' ${shell.escape script}
      echo 'reply_404'
    } > $out/bin/htgen
    chmod +x $out/bin/htgen
  '';
}
