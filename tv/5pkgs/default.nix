{ pkgs, ... }:

{
  ff = pkgs.callPackage ./ff {};
  viljetic-pages = pkgs.callPackage ./viljetic-pages {};
}
