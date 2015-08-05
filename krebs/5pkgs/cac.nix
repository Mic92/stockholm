{ stdenv, fetchgit, coreutils, curl, gnused, jq, sshpass, ... }:

stdenv.mkDerivation {
  name = "cac";

  src = fetchgit {
    url = http://cgit.cd.retiolum/cac;
    rev = "0fc9cbeba4060380f698f51bb74081e2fcefadf3";
    sha256 = "9759c78aa9aa04ab82486d0f24264bff1081513bc07cac0f8b3c0bdf52260fb3";
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
        sshpass
      ];
    in
    ''
      mkdir -p $out/bin

      sed \
        's,^\(  true) \)\(cac "$@";;\)$,\1 PATH=${path} \2,' \
        < ./cac \
        > $out/bin/cac

      chmod +x $out/bin/cac
    '';
}
