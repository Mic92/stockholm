with import ./lib;

self: super:

{
  rpiPackages = lib.mapNixDir (path: self.callPackage path {}) ./.;
}
