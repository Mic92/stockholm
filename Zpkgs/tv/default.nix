{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
  krebs = import ../../Zpkgs/krebs { inherit pkgs; };
in

krebs // {
  charybdis = callPackage ./charybdis {};
  dic = callPackage ./dic.nix {};
  lentil = callPackage ./lentil {};
  much = callPackage ./much.nix {};
  viljetic-pages = callPackage ./viljetic-pages {};
}
