#!/bin/sh
set -efux

disk=$1

export NIXPKGS_ALLOW_UNFREE=1
stockholm_root=$(git rev-parse --show-toplevel)
ssh root@localhost -t -- $(nix-build \
  --no-out-link \
  -I nixpkgs=/var/src/nixpkgs \
  -I stockholm="$stockholm_root" \
  -I secrets="$stockholm_root"/lass/2configs/tests/dummy-secrets \
  -E "with import <nixpkgs> {}; (pkgs.nixos [ { mainDisk = \"$disk\"; disko.rootMountPoint = \"/mnt/hilum\"; } ./physical.nix ]).mountScript"
)
$(nix-build \
  --no-out-link \
  -I nixpkgs=/var/src/nixpkgs \
  "$stockholm_root"/lass/krops.nix -A populate \
  --argstr name hilum \
  --argstr target "root@localhost/mnt/hilum/var/src" \
  --arg force true
)
ssh root@localhost << SSH
nixos-install --no-root-password --root /mnt/hilum -I /var/src
nixos-enter --root /mnt/hilum -- nixos-rebuild -I /var/src switch --install-bootloader
umount -Rv /mnt/hilum
SSH
