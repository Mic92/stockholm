{ config, pkgs, ... }:
with import <stockholm/lib>;
{
  nixpkgs.config.packageOverrides = super: let

    # This callPackage will try to detect obsolete overrides.
    callPackage = path: args: let
      override = super.callPackage path args;
      upstream = optionalAttrs (override ? "name")
        (super.${(parseDrvName override.name).name} or {});
    in if upstream ? "name" &&
          override ? "name" &&
          compareVersions upstream.name override.name != -1
      then trace "Upstream `${upstream.name}' gets overridden by `${override.name}'." override
      else override;

  in {}
  // mapAttrs (_: flip callPackage {})
              (filterAttrs (_: dir: pathExists (dir + "/default.nix"))
                           (subdirsOf ./.))
  // {
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
    ejabberd = callPackage ./ejabberd {
      erlang = pkgs.erlangR16;
    };
    ff = pkgs.writeDashBin "ff" ''
      exec ${pkgs.firefoxWrapper}/bin/firefox "$@"
    '';
    gnupg =
      if elem config.krebs.build.host.name ["xu" "wu"]
        then super.gnupg21
        else super.gnupg;
  };
}
