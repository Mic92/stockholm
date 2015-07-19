{ pkgs, ... }:

pkgs //
{
  dic = pkgs.callPackage ./dic.nix {};
  much = pkgs.callPackage ./much.nix {};
}
