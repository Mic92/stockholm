{ coreutils, gnugrep, gnused, fetchgit, jq, nix, stdenv, ... }:

stdenv.mkDerivation {
  name = "get-1.2.0";

  src = fetchgit {
    url = http://cgit.cd.retiolum/get;
    rev = "9801ebe6f527b9505799ff423c427c03694d85de";
    sha256 = "278dee0b873907650b97cc95a60c26f027ed59d75d9c4c23e9667a352ea60eea";
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
