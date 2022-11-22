{ lib, pkgs, ... }:

pkgs.writers.writeDashBin "hashPassword" ''
  # usage: hashPassword [...]
  set -euf

  export PATH=${lib.makeBinPath (with pkgs; [
    coreutils
    mkpasswd
    openssl
  ])}

  salt=$(openssl rand -base64 16 | tr -d '+=' | head -c 16)
  exec mkpasswd -m sha-512 -S "$salt" "$@"
''
