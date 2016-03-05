{ fetchgit, lib, stdenv
, coreutils
, git
, gnumake
, gnused
, jq
, nix
, openssh
, parallel
, ... }:

stdenv.mkDerivation {
  name = "push-1.1.2";

  src = fetchgit {
    url = http://cgit.cd.krebsco.de/push;
    rev = "da5b3a4b05ef822cc41d36b6cc2071a2e78506d4";
    sha256 = "0gfxz207lm11g77rw02jcqpvzhx07j9hzgjgscbmslzl5r8icd6g";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase =
    let
      path = lib.makeBinPath [
        coreutils
        git
        gnumake
        gnused
        jq
        nix
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
