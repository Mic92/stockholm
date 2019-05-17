{ pkgs, stdenv, ... }:

stdenv.mkDerivation rec {
  name = "github-hosts-sync-${version}";
  version = "2.0.0";

  src = ./src;

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = let
    ca-bundle = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
    path = stdenv.lib.makeBinPath [
      pkgs.git
      pkgs.nettools
      pkgs.openssh
      pkgs.rsync
    ];
  in
    ''
      mkdir -p $out/bin

      cp hosts-sync $out/bin/github-hosts-sync

      sed -i \
        '1s,$,\nPATH=${path}''${PATH+:$PATH} GIT_SSL_CAINFO=${ca-bundle},' \
        $out/bin/github-hosts-sync
    '';
}
