{ bash, coreutils, gnused, stdenv, fetchgit, ucspi-tcp }:
with import <stockholm/lib>;
let
  version = "1.2";
in stdenv.mkDerivation {
  name = "htgen-${version}";

  src = fetchgit {
    url = "http://cgit.krebsco.de/htgen";
    rev = "refs/tags/v${version}";
    sha256 = "0y7gi4r32dvc18a4nnkr74sbq4glqcmf1q6lfj8fpgj82lg16zc6";
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
