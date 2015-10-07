{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
in

rec {
  bitlbee-dev = callPackage ./bitlbee-dev.nix {};
  bitlbee-steam = callPackage ./bitlbee-steam.nix { inherit bitlbee-dev; };
  bitlbee = callPackage ./bitlbee.nix { inherit bitlbee-steam; };
  firefoxPlugins = {
    noscript = callPackage ./firefoxPlugins/noscript.nix {};
    ublock = callPackage ./firefoxPlugins/ublock.nix {};
    vimperator = callPackage ./firefoxPlugins/vimperator.nix {};
  };
}
