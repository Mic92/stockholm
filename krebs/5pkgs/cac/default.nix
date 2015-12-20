{ stdenv, fetchgit, coreutils, curl, gnused, inotifyTools, jq, ncurses, sshpass, ... }:

stdenv.mkDerivation {
  name = "cac-1.0.2";

  src = fetchgit {
    url = http://cgit.cd.retiolum/cac;
    rev = "3d6aef3631aa2e0becba447ed9c36a268dcf8bb5";
    sha256 = "4f584ef8d53a003818ec6608d2cccda42fc7806cd6f9fa9ad179346f3f59744c";
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

      sed 's;^_main .*;PATH=${path} &;' < ./cac > $out/bin/cac

      chmod +x $out/bin/cac
    '';
}
