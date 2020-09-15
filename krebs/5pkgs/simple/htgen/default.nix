{ coreutils, dash, fetchgit, gnused, stdenv, ucspi-tcp }:
with import <stockholm/lib>;
let
  version = "1.2.8";
in stdenv.mkDerivation {
  name = "htgen-${version}";

  src = fetchgit {
    url = "http://cgit.krebsco.de/htgen";
    rev = "refs/tags/v${version}";
    sha256 = "046c05jswar2agagqixad3idqxca494aaf199h6bdn02cyzygnpq";
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
