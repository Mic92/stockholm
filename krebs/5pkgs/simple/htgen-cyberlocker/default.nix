{ pkgs, stockholm, stdenv }:
with stockholm.lib;

stdenv.mkDerivation rec {
  pname = "htgen-cyberlocker";
  version = "1.0.0";

  src = ./src;

  buildPhase = ''
    (
      exec > htgen-cyberlocker
      echo PATH=${makeBinPath [
        pkgs.coreutils
        pkgs.file
        pkgs.findutils
        pkgs.gnugrep
        pkgs.jq
        pkgs.nix
        pkgs.util-linux
      ]}
      echo STATEDIR=${shell.escape "\${STATEDIR-$HOME}"}
      cat $src/htgen-cyberlocker
    )
  '';

  installPhase = ''
    install -D htgen-cyberlocker $out/bin/htgen-cyberlocker
  '';
}
