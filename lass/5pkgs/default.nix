{ pkgs, ... }@args:

{
  nixpkgs.config.packageOverrides = rec {
    acronym = pkgs.callPackage ./acronym/default.nix {};
    ejabberd = pkgs.callPackage ./ejabberd {
      erlang = pkgs.erlangR16;
    };
    firefoxPlugins = {
      noscript = pkgs.callPackage ./firefoxPlugins/noscript.nix {};
      ublock = pkgs.callPackage ./firefoxPlugins/ublock.nix {};
      vimperator = pkgs.callPackage ./firefoxPlugins/vimperator.nix {};
    };
    init = pkgs.callPackage ./init/default.nix args;
    logify = pkgs.callPackage ./logify/default.nix {};
    mk_sql_pair = pkgs.callPackage ./mk_sql_pair/default.nix {};
    mpv-poll = pkgs.callPackage ./mpv-poll/default.nix {};
    pop = pkgs.callPackage ./pop/default.nix {};
    q = pkgs.callPackage ./q {};
    rs = pkgs.callPackage ./rs/default.nix {};
    untilport = pkgs.callPackage ./untilport/default.nix {};
    urban = pkgs.callPackage ./urban/default.nix {};
    xmonad-lass = import ./xmonad-lass.nix { inherit pkgs; };
    yt-next = pkgs.callPackage ./yt-next/default.nix {};
  };
}
