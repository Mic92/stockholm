with import <stockholm/lib>;

self: super:

# Import files and subdirectories like they are overlays.
foldl' mergeAttrs {}
  (map
    (name: import (./. + "/${name}") self super)
    (filter
      (name: name != "default.nix" && !hasPrefix "." name)
      (attrNames (readDir ./.))))

//

{
  brscan4 = overrideDerivation super.brscan4 (original: rec {
    name = "brscan4-0.4.4-4";
    src = super.fetchurl {
      url = "http://download.brother.com/welcome/dlf006645/${name}.amd64.deb";
      sha256 = "0xy5px96y1saq9l80vwvfn6anr2q42qlxdhm6ci2a0diwib5q9fd";
    };
  });

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

  gitAndTools = super.gitAndTools // {
    inherit (self) diff-so-fancy;
  };

  ff = self.writeDashBin "ff" ''
    exec ${self.firefoxWrapper}/bin/firefox "$@"
  '';

  gnupg = self.gnupg22;

  # https://github.com/NixOS/nixpkgs/issues/16113
  wvdial = let
    nixpkgs-1509 = import (self.fetchFromGitHub {
      owner = "NixOS"; repo = "nixpkgs-channels";
      rev = "91371c2bb6e20fc0df7a812332d99c38b21a2bda";
      sha256 = "1as1i0j9d2n3iap9b471y4x01561r2s3vmjc5281qinirlr4al73";
    }) {};
  in nixpkgs-1509.wvdial;
}
