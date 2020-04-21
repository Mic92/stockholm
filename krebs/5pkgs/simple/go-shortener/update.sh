#!/usr/bin/env nix-shell
#! nix-shell -i bash -p nodePackages_10_x.node2nix
node2nix -10 -i pkgs.json -c combine.nix
rm node-env.nix combine.nix
