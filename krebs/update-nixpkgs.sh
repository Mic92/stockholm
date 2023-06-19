#!/bin/sh
dir=$(dirname $0)
oldrev=$(cat $dir/nixpkgs.json | jq -r .rev | sed 's/\(.\{7\}\).*/\1/')
nix-shell -p nix-prefetch-git --run 'nix-prefetch-git \
  --url https://github.com/NixOS/nixpkgs \
  --rev refs/heads/nixos-23.05' \
> $dir/nixpkgs.json
newrev=$(cat $dir/nixpkgs.json | jq -r .rev | sed 's/\(.\{7\}\).*/\1/')
git commit $dir/nixpkgs.json -m "nixpkgs: $oldrev -> $newrev"
