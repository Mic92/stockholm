{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
in

rec {
  #bitlbee-dev = callPackage ./bitlbee-dev.nix {};
  #bitlbee-steam = callPackage ./bitlbee-steam.nix { inherit bitlbee-dev; };
  #bitlbee = callPackage ./bitlbee.nix { inherit bitlbee-steam; };
  firefoxPlugins = {
    noscript = callPackage ./firefoxPlugins/noscript.nix {};
    ublock = callPackage ./firefoxPlugins/ublock.nix {};
    vimperator = callPackage ./firefoxPlugins/vimperator.nix {};
  };
  newsbot-js = callPackage ./newsbot-js/default.nix {};
  xmonad-lass =
    let src = pkgs.writeNixFromCabal "xmonad-lass.nix" ./xmonad-lass; in
    pkgs.haskellPackages.callPackage src {};
}
