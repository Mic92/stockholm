{ pkgs, ... }:

pkgs //
{
  much = pkgs.callPackage ./much.nix {};
}
