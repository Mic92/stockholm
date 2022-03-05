with import <stockholm/lib>;
let
  pushBack = x: xs:
    if elem x xs then
      remove x xs ++ [ x ]
    else
      names;
in

self: super:

# Import files and subdirectories like they are overlays.
fix
  (foldl' (flip extends) (_: super)
    (map
      (name: import (./. + "/${name}"))
      (filter
        (name: name != "default.nix" && !hasPrefix "." name)
        (pushBack "override"
          (attrNames (readDir ./.))))))

//

{
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

}
