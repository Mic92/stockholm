{ stdenv, fetchgit, coreutils, curl, gnused, gnugrep, ... }:

stdenv.mkDerivation {
  name = "dic";

  src = fetchgit {
    url = http://cgit.ni.krebsco.de/dic;
    rev = "refs/tags/v1.0.1";
    sha256 = "1686mba1z4m7vq70w26qpl00z1cz286c9bya9ql36g6w2pbcs8d3";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase =
    let
      path = stdenv.lib.makeBinPath [
        coreutils
        curl
        gnused
        gnugrep
      ];
    in
    ''
      mkdir -p $out/bin

      sed \
        's,^main() {$,&\n  PATH=${path}; export PATH,' \
        < ./dic \
        > $out/bin/dic

      chmod +x $out/bin/dic
    '';
}
