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
  name = "push-1.0.0";

  src = fetchgit {
    url = http://cgit.cd.retiolum/push;
    rev = "513da89fe50b3bad3d758855f5622c4508977e4a";
    sha256 = "6124e1d4d4ef57455e2f06891e06fb01d3786846efaf9b79e3176d89988e1b4e";
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

