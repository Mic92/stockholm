{ fetchgit, lib, stdenv
, coreutils
, get
, git
, gnused
, jq
, openssh
, parallel
, ... }:

stdenv.mkDerivation {
  name = "push-1.1.1";

  src = fetchgit {
    url = http://cgit.cd.retiolum/push;
    rev = "ea8b76569c6b226fe148e559477669b095408472";
    sha256 = "c305a1515d30603f6ed825d44487e863fdc7d90400620ceaf2c335a3b5d1e221";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase =
    let
      path = lib.makeSearchPath "bin" [
        coreutils
        get
        git
        gnused
        jq
        openssh
        parallel
      ];
    in
    ''
      mkdir -p $out/bin

      sed \
        '1s,.*,&\nPATH=${path},' \
        < ./push \
        > $out/bin/push

      chmod +x $out/bin/push
    '';
}
