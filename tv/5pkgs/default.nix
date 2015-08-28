{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
in

{
  charybdis = callPackage ./charybdis {};
  lentil = callPackage ./lentil {};
  much = callPackage ./much.nix {};
  viljetic-pages = callPackage ./viljetic-pages {};
}
