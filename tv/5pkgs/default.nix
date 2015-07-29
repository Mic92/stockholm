{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
  kpkgs = import ../../krebs/5pkgs { inherit pkgs; };
in

kpkgs // {
  charybdis = callPackage ./charybdis {};
  lentil = callPackage ./lentil {};
  much = callPackage ./much.nix {};
  viljetic-pages = callPackage ./viljetic-pages {};
}
