{ pkgs, ... }:

{
  cr = pkgs.writeScriptBin "cr" ''
    #! /bin/sh
    set -efu
    export LC_TIME=de_DE.utf8
    exec ${pkgs.chromium}/bin/chromium \
        --ssl-version-min=tls1 \
        --disk-cache-dir=/tmp/chromium-disk-cache_"$LOGNAME" \
        --disk-cache-size=50000000 \
        "%@"
  '';
  ff = pkgs.callPackage ./ff {};
  viljetic-pages = pkgs.callPackage ./viljetic-pages {};
  xmonad-tv =
    let src = pkgs.writeNixFromCabal "xmonad-tv.nix" ./xmonad-tv; in
    pkgs.haskellPackages.callPackage src {};
}
