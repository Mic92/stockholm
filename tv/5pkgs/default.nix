with import <stockholm/lib>;
self: super: let

  # This callPackage will try to detect obsolete overrides.
  callPackage = path: args: let
    override = super.callPackage path args;
    upstream = optionalAttrs (override ? "name")
      (super.${(parseDrvName override.name).name} or {});
  in if upstream ? "name" &&
        override ? "name" &&
        compareVersions upstream.name override.name != -1
    then
      trace
        "Upstream `${upstream.name}' gets overridden by `${override.name}'."
        override
    else override;

in {

  # TODO use XDG_RUNTIME_DIR?
  cr = self.writeDashBin "cr" ''
    set -efu
    export LC_TIME=de_DE.utf8
    exec ${self.chromium}/bin/chromium \
        --ssl-version-min=tls1 \
        --disk-cache-dir=/tmp/chromium-disk-cache_"$LOGNAME" \
        --disk-cache-size=50000000 \
        "$@"
  '';

  ff = self.writeDashBin "ff" ''
    exec ${self.firefoxWrapper}/bin/firefox "$@"
  '';

  gnupg = self.gnupg21;

  # https://github.com/NixOS/nixpkgs/issues/16113
  wvdial = let
    nixpkgs-1509 = import (self.fetchFromGitHub {
      owner = "NixOS"; repo = "nixpkgs-channels";
      rev = "91371c2bb6e20fc0df7a812332d99c38b21a2bda";
      sha256 = "1as1i0j9d2n3iap9b471y4x01561r2s3vmjc5281qinirlr4al73";
    }) {};
  in nixpkgs-1509.wvdial;

}

// mapAttrs (_: flip callPackage {})
            (filterAttrs (_: dir: pathExists (dir + "/default.nix"))
                         (subdirsOf ./.))
