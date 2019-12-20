#! /usr/bin/env nix-shell
#! nix-shell -i bash -p nodePackages.node2nix
node2nix -12 -i pkgs.json -c combine.nix
rm node-env.nix combine.nix
