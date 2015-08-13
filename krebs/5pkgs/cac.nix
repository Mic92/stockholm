{ stdenv, fetchgit, coreutils, curl, gnused, jq, ncurses, sshpass, ... }:

stdenv.mkDerivation {
  name = "cac";

  src = fetchgit {
    url = http://cgit.cd.retiolum/cac;
    rev = "f4589158572ab35969b9bccf801ea07e115705e1";
    sha256 = "9d761cd1d7ff68507392cbfd6c3f6000ddff9cc540293da2b3c4ee902321fb27";
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
