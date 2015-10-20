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
  name = "push-1.1.0";

  src = fetchgit {
    url = http://cgit.cd.retiolum/push;
    rev = "c5f4bda5bd00bad7778bbd5a9af8d476de0de920";
    sha256 = "d335b644b791214263cee5c6659538c8e45326531b0588e5e7eb3bd9ef969800";
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

