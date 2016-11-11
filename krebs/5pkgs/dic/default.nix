{ stdenv, fetchgit, coreutils, curl, gnused, gnugrep, ... }:

stdenv.mkDerivation {
  name = "dic";

  src = fetchgit {
    url = http://cgit.ni.krebsco.de/dic;
    rev = "refs/tags/v1.0.0";
    sha256 = "0f3f5dqpw5y79p2k68qw6jdlkrnapqs3nvnc41zwacyhgppiww0k";
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
