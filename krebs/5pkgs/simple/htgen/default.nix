{ coreutils, dash, fetchgit, gnused, stdenv, ucspi-tcp }:
with import <stockholm/lib>;
let
  version = "1.2.3";
in stdenv.mkDerivation {
  name = "htgen-${version}";

  src = fetchgit {
    url = "http://cgit.krebsco.de/htgen";
    rev = "refs/tags/v${version}";
    sha256 = "0lml336w31ckgspp633ym2jnppzln3f8mvmy3y2vz9yanf59j0hb";
  };

  installPhase = ''
    mkdir -p $out/bin
    {
      echo '#! ${dash}/bin/dash'
      echo 'export PATH=${makeBinPath [
        coreutils
        gnused
        ucspi-tcp
      ]}''${PATH+":$PATH"}'
      sed 's:^Server=htgen$:&/${version}:' htgen
    } > $out/bin/htgen
    chmod +x $out/bin/htgen
    cp -r examples $out
  '';
}
