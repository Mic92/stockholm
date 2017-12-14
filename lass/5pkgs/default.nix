{ config, pkgs, ... }@args:

{
  nixpkgs.config.packageOverrides = rec {
    acronym = pkgs.callPackage ./acronym/default.nix {};
    dpass = pkgs.callPackage ./dpass {};
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
    urban = pkgs.callPackage ./urban/default.nix {};
    xml2json = pkgs.callPackage ./xml2json/default.nix {};
    xmonad-lass = import ./xmonad-lass.nix { inherit config pkgs; };
    yt-next = pkgs.callPackage ./yt-next/default.nix {};

    screengrab = pkgs.writeDashBin "screengrab" ''
      resolution="$(${pkgs.xorg.xrandr}/bin/xrandr | ${pkgs.gnugrep}/bin/grep '*' | ${pkgs.gawk}/bin/awk '{print $1}')"
      ${pkgs.ffmpeg}/bin/ffmpeg -f x11grab -r 25 -i :${toString config.services.xserver.display} -s $resolution -c:v huffyuv $1
    '';
  };
}
