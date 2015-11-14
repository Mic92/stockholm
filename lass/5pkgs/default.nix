{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
in

rec {
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
