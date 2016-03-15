{ pkgs, ... }:

{
  nixpkgs.config.packageOverrides = rec {
    acronym = pkgs.callPackage ./acronym/default.nix {};
    firefoxPlugins = {
      noscript = pkgs.callPackage ./firefoxPlugins/noscript.nix {};
      ublock = pkgs.callPackage ./firefoxPlugins/ublock.nix {};
      vimperator = pkgs.callPackage ./firefoxPlugins/vimperator.nix {};
    };
    newsbot-js = pkgs.callPackage ./newsbot-js/default.nix {};
    xmonad-lass =
      let src = pkgs.writeNixFromCabal "xmonad-lass.nix" ./xmonad-lass; in
      pkgs.haskellPackages.callPackage src {};
  };
}
