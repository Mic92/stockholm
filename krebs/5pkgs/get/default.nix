{ coreutils, gnugrep, gnused, fetchgit, jq, nix, stdenv, ... }:

stdenv.mkDerivation {
  name = "get-1.3.0";

  src = fetchgit {
    url = http://cgit.cd.retiolum/get;
    rev = "fbe8f8d12ede9762fceb15b9944b69a4ee6331eb";
    sha256 = "bcdf036f8b5d1467285d0998aeac7e48280adfb9e1278f9f424c9c8b5e6ed8fa";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase =
    let
      path = stdenv.lib.makeSearchPath "bin" [
        coreutils
        gnugrep
        gnused
        jq
        nix
      ];
    in
    ''
      mkdir -p $out/bin

      sed \
        '1s,.*,&\nPATH=${path},' \
        < ./get \
        > $out/bin/get

      chmod +x $out/bin/get
    '';
}
