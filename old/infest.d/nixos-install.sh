#! /bin/sh
# usage: cat infest-nixos-install.sh | ./cac ssh ...
set -euf
nixos-install \
    -I secrets=/etc/nixos/secrets \
    -I retiolum-hosts=/etc/nixos/hosts \
    -I pubkeys=/etc/nixos/pubkeys \
    -I nixpkgs=/etc/nixos/nixpkgs
