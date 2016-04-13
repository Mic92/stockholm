{ pkgs, ... }:

{
  nixpkgs.config.packageOverrides = rec {
    acronym = pkgs.callPackage ./acronym/default.nix {};
    firefoxPlugins = {
      noscript = pkgs.callPackage ./firefoxPlugins/noscript.nix {};
      ublock = pkgs.callPackage ./firefoxPlugins/ublock.nix {};
      vimperator = pkgs.callPackage ./firefoxPlugins/vimperator.nix {};
    };
    mk_sql_pair = pkgs.callPackage ./mk_sql_pair/default.nix {};
    mpv-poll = pkgs.callPackage ./mpv-poll/default.nix {};
    xmonad-lass =
      let src = pkgs.writeNixFromCabal "xmonad-lass.nix" ./xmonad-lass; in
      pkgs.haskellPackages.callPackage src {};
    yt-next = pkgs.callPackage ./yt-next/default.nix {};
  };
}
