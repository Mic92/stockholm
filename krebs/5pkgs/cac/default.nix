{ stdenv, fetchgit, coreutils, curl, gnused, inotifyTools, jq, ncurses, sshpass, ... }:

stdenv.mkDerivation {
  name = "cac-1.0.0";

  src = fetchgit {
    url = http://cgit.cd.retiolum/cac;
    rev = "14de1d3c78385e3f8b6d694f5d799eb1b613159e";
    sha256 = "9b2a3d47345d6f8f27d9764c4f2f2acff17d3dde145dd0e674e4183e9312fec3";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase =
    let
      path = stdenv.lib.makeSearchPath "bin" [
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

      sed \
        's,^\(  true) \)\(cac "$@";;\)$,\1 PATH=${path}${PATH+:$PATH} \2,' \
        < ./cac \
        > $out/bin/cac

      chmod +x $out/bin/cac
    '';
}
