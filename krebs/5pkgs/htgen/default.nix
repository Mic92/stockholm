{ bash, coreutils, gnused, stdenv, fetchgit, ucspi-tcp }:
with import <stockholm/lib>;
let
  version = "1.1";
in stdenv.mkDerivation {
  name = "htgen-${version}";

  src = fetchgit {
    url = "http://cgit.krebsco.de/htgen";
    rev = "refs/tags/v${version}";
    sha256 = "1zxj0fv9vdrqyl3x2hgq7a6xdlzpclf93akygysrzsqk9wjapp4z";
  };

  installPhase = ''
    mkdir -p $out/bin
    {
      echo '#! ${bash}/bin/bash'
      echo 'export PATH=${makeBinPath [
        ucspi-tcp
        coreutils
        gnused
      ]}''${PATH+":$PATH"}'
      cat htgen
    } > $out/bin/htgen
    chmod +x $out/bin/htgen
    cp -r examples $out
  '';
}
