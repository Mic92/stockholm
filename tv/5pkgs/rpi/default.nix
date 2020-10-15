let
  lib = import <stockholm/lib>;
in

self: super:

{
  rpiPackages = lib.mapNixDir (path: self.callPackage path {}) ./.;
}
