#!/bin/sh
dir=$(dirname $0)
oldrev=$(cat $dir/nixpkgs-unstable.json | jq -r .rev | sed 's/\(.\{7\}\).*/\1/')
nix-shell -p nix-prefetch-git --run 'nix-prefetch-git \
  --url https://github.com/NixOS/nixpkgs \
  --rev refs/heads/nixos-unstable' \
> $dir/nixpkgs-unstable.json
newrev=$(cat $dir/nixpkgs-unstable.json | jq -r .rev | sed 's/\(.\{7\}\).*/\1/')
git commit $dir/nixpkgs-unstable.json -m "nixpkgs-unstable: $oldrev -> $newrev"
