{ stdenv, fetchgit, coreutils, curl, gnused, jq, sshpass, ... }:

stdenv.mkDerivation {
  name = "cac";

  src = fetchgit {
    url = http://cgit.cd.retiolum/cac;
    rev = "07ef31c50613634e88a31233d1fcd2ec3e52bfe8";
    sha256 = "4e94709a3f580a53983ca418fa0b470817ac917aa1b2d095f2420afd36ea9158";
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
