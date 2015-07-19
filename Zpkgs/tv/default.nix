{ pkgs, ... }:

pkgs //
{
  dic = pkgs.callPackage ./dic.nix {};
  github-known_hosts = pkgs.callPackage ./github-known_hosts.nix {};
  much = pkgs.callPackage ./much.nix {};
}
