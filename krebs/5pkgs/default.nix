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
  # https://github.com/NixOS/nixpkgs/pull/30065
  brscan4 = overrideDerivation super.brscan4 (original: rec {
    name = "brscan4-0.4.4-4";
    src = super.fetchurl {
      url = "http://download.brother.com/welcome/dlf006645/${name}.amd64.deb";
      sha256 = "0xy5px96y1saq9l80vwvfn6anr2q42qlxdhm6ci2a0diwib5q9fd";
    };
  });

  reaktor2 = self.haskellPackages.reaktor2;

  ReaktorPlugins = self.callPackage ./simple/Reaktor/plugins.nix {};

  # https://github.com/proot-me/PRoot/issues/106
  proot = self.writeDashBin "proot" ''
    export PROOT_NO_SECCOMP=1
    exec ${super.proot}/bin/proot "$@"
  '';
}
