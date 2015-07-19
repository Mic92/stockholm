{ stdenv, fetchgit, pkgs, ... }:

stdenv.mkDerivation {
  name = "github-hosts-sync";

  src = fetchgit {
    url = https://github.com/krebscode/painload;
    rev = "35ccac73d563ad30d2851b9aeed4cfef69ff74e3";
    sha256 = "1y1fs2p3xj2yrqpw0h5kd0f3c5p1y70xk1hjnw99sr33r67s9c35";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase =
    let
      ca-bundle = "${pkgs.cacert}/etc/ca-bundle.crt";
      path = stdenv.lib.makeSearchPath "bin" (with pkgs; [
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
