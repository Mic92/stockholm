{ stdenv, fetchgit, bc, coreutils, curl, gnused, inotifyTools, jq, ncurses, sshpass, ... }:

stdenv.mkDerivation {
  name = "cac-api-1.1.0";

  src = fetchgit {
    url = http://cgit.cd.krebsco.de/cac-api;
    rev = "0809fae379239687ed1170e04311dc2880ef0aba";
    sha256 = "357ced27c9ed88028967c934178a1d230bf38617a7494cd4632fabdd2a04fcdd";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase =
    let
      path = stdenv.lib.makeSearchPath "bin" [
        bc
        coreutils
        curl
        gnused
        inotifyTools
        jq
        ncurses
        sshpass
      ];
    in
    ''
      mkdir -p $out/bin
      cp cac-api $out/bin/cac-api
      sed -i '
        s;^_cac_cli_main .*;PATH=${path}''${PATH+:$PATH} &;
      ' $out/bin/cac-api
    '';
}
