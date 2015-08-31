{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
  kpkgs = import ../../krebs/5pkgs { inherit pkgs; };
in

kpkgs //
rec {
  bitlbee-dev = callPackage ./bitlbee-dev.nix {};
  bitlbee-steam = callPackage ./bitlbee-steam.nix { inherit bitlbee-dev; };
  bitlbee = callPackage ./bitlbee.nix { inherit bitlbee-steam; };
}
