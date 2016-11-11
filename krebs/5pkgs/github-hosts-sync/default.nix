{ pkgs, stdenv, ... }:

stdenv.mkDerivation {
  name = "github-hosts-sync";

  src = pkgs.painload;

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase =
    let
      ca-bundle = "${pkgs.cacert}/etc/ssl/certs/ca-bundle.crt";
      path = stdenv.lib.makeBinPath (with pkgs; [
        coreutils
        findutils
        git
        gnugrep
        gnused
        openssh
        socat
      ]);
    in
    ''
      mkdir -p $out/bin

      sed \
        's,^main() {$,&\n  export PATH=${path} GIT_SSL_CAINFO=${ca-bundle},' \
        < ./retiolum/scripts/github_hosts_sync/hosts-sync \
        > $out/bin/github-hosts-sync

      chmod +x $out/bin/github-hosts-sync
    '';
}
