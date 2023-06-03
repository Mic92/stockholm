{ pkgs, stockholm, stdenv }:
with stockholm.lib;

stdenv.mkDerivation rec {
  pname = "htgen-paste";
  version = "1.0.0";

  src = ./src;

  buildPhase = ''
    (
      exec > htgen-paste
      echo PATH=${makeBinPath [
        pkgs.nix
        pkgs.file
        pkgs.coreutils
        pkgs.findutils
      ]}
      echo STATEDIR=${shell.escape "\${STATEDIR-$HOME}"}
      cat $src/htgen-paste
    )
  '';

  installPhase = ''
    install -D htgen-paste $out/bin/htgen-paste
  '';
}
