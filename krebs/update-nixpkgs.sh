#!/bin/sh
dir=$(dirname $0)
oldrev=$(cat $dir/nixpkgs.json | jq -r .rev | sed 's/\(.\{7\}\).*/\1/')
nix-shell -p nix-prefetch-git --run 'nix-prefetch-git \
  --url https://github.com/NixOS/nixpkgs-channels \
  --rev refs/heads/nixos-19.09' \
> $dir/nixpkgs.json
newrev=$(cat $dir/nixpkgs.json | jq -r .rev | sed 's/\(.\{7\}\).*/\1/')
git commit $dir/nixpkgs.json -m "nixpkgs: $oldrev -> $newrev"
