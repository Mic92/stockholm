{ coreutils, dash, fetchgit, gnused, stdenv, ucspi-tcp }:
with import <stockholm/lib>;
let
  version = "1.2.7";
in stdenv.mkDerivation {
  name = "htgen-${version}";

  src = fetchgit {
    url = "http://cgit.krebsco.de/htgen";
    rev = "refs/tags/v${version}";
    sha256 = "1jnpr7f2mgsr8n2nz9sa69v5v8ay1jjz4ydbnq14vrpf5q9qq9fx";
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
