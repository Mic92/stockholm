{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  nixpkgs.config.packageOverrides = super: {
    # TODO use XDG_RUNTIME_DIR?
    cr = pkgs.writeDashBin "cr" ''
      set -efu
      export LC_TIME=de_DE.utf8
      exec ${pkgs.chromium}/bin/chromium \
          --ssl-version-min=tls1 \
          --disk-cache-dir=/tmp/chromium-disk-cache_"$LOGNAME" \
          --disk-cache-size=50000000 \
          "$@"
    '';
    ejabberd = pkgs.callPackage ./ejabberd {
      erlang = pkgs.erlangR16;
    };
    ff = pkgs.writeDashBin "ff" ''
      exec ${pkgs.firefoxWrapper}/bin/firefox "$@"
    '';
    gnupg =
      if elem config.krebs.build.host.name ["xu" "wu"]
        then super.gnupg21
        else super.gnupg;
    q = pkgs.callPackage ./q {};
    viljetic-pages = pkgs.callPackage ./viljetic-pages {};
    xmonad-tv = import ./xmonad-tv.nix { inherit pkgs; };
  };
}
