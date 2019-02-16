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

  dhcpcd = overrideDerivation super.dhcpcd (old: {
    configureFlags = old.configureFlags ++ [
      "--dbdir=/var/lib/dhcpcd"
    ];
  });

  gitAndTools = super.gitAndTools // {
    inherit (self) diff-so-fancy;
  };

  ff = self.writeDashBin "ff" ''
    exec ${self.firefoxWrapper}/bin/firefox "$@"
  '';

  gnupg = self.gnupg22;

  pass = {
    "18.03" =
      self.callPackage ./compat/18.03/pass {
        pass-otp = self.callPackage ./compat/18.03/pass-otp {};
      };
    "18.09" =
      super.pass.withExtensions (ext: [
        ext.pass-otp
      ]);
  }.${versions.majorMinor nixpkgsVersion};

}
