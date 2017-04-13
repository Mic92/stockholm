{ bash, coreutils, gnused, stdenv, fetchgit, ucspi-tcp }:
with import <stockholm/lib>;
let
  version = "1.2.1";
in stdenv.mkDerivation {
  name = "htgen-${version}";

  src = fetchgit {
    url = "http://cgit.krebsco.de/htgen";
    rev = "refs/tags/v${version}";
    sha256 = "0mh8n9hf4jmkcxwdw3rimwdlhslrnpsdg1cp2vq3h9j2jkrsq6ba";
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
