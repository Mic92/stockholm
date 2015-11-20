{ pkgs, ... }:

{
  # TODO use XDG_RUNTIME_DIR?
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
  ff = pkgs.writeScriptBin "ff" ''
    #! /bin/sh
    set -efu
    exec ${pkgs.firefoxWrapper}/bin/firefox $(printf " %q" "$@")
  '';
  xmonad-tv =
    let src = pkgs.writeNixFromCabal "xmonad-tv.nix" ./xmonad-tv; in
    pkgs.haskellPackages.callPackage src {};
}
