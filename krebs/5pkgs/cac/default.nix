{ stdenv, fetchgit, coreutils, curl, gnused, inotifyTools, jq, ncurses, sshpass, ... }:

stdenv.mkDerivation {
  name = "cac-1.0.0";

  src = fetchgit {
    url = http://cgit.gum/cac;
    rev = "fe3b2ecb0aaf7d863842b896e18cd2b829f2297b";
    sha256 = "05bnd7wyjhqy8srmpnc8d234rv3jxdjgb4z0hlfb9kg7mb12w1ya";
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
