{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
in

{
  viljetic-pages = callPackage ./viljetic-pages {};
}
