{ stdenv, fetchgit, bc, coreutils, curl, gnused, inotifyTools, jq, ncurses, sshpass, ... }:

stdenv.mkDerivation {
  name = "cac-1.0.3";

  src = fetchgit {
    url = http://cgit.cd.retiolum/cac;
    rev = "22acc1b990ac7d97c16344fbcbc2621e24cdf915";
    sha256 = "135b740617c983b3f46a1983d4744be17340d5146a0a0de0dff4bb7a53688f2f";
  };

  phases = [
    "unpackPhase"
    "patchPhase"
    "installPhase"
  ];
  patches = [ ./disable-tasks.patch ];
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

      sed < ./cac > $out/bin/cac '
        s;^_cac_main .*;PATH=${path}''${PATH+:$PATH} &;
      '

      chmod +x $out/bin/cac
    '';
}
