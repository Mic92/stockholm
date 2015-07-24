{ lib, pkgs, ... }:

pkgs.writeScriptBin "genid" ''
  #! /bin/sh
  # usage: genid NAME
  set -euf

  export PATH=${lib.makeSearchPath "bin" (with pkgs; [
    bc
    coreutils
  ])}

  name=$1
  hash=$(printf %s "$name" | sha1sum | cut -d\  -f1 | tr a-f A-F)
  echo "
    min=2^24  # bigger than nobody and nogroup, see <nixos/modules/misc/ids.nix>
              # and some spare for stuff like lxd.
    max=2^32  # see 2^(8*sizeof(uid_t))
    ibase=16
    ($hash + min) % max
  " | bc
''
