{ pkgs, ... }:

{
  ff = pkgs.callPackage ./ff {};
  viljetic-pages = pkgs.callPackage ./viljetic-pages {};
  xmonad-tv =
    let src = pkgs.writeNixFromCabal "xmonad-tv.nix" ./xmonad-tv; in
    pkgs.haskellPackages.callPackage src {};
}
