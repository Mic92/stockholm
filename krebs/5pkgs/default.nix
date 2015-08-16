{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
in

pkgs //
{
  cac = callPackage ./cac.nix {};
  dic = callPackage ./dic.nix {};
  genid = callPackage ./genid.nix {};
  github-hosts-sync = callPackage ./github-hosts-sync.nix {};
  github-known_hosts = callPackage ./github-known_hosts.nix {};
  hashPassword = callPackage ./hashPassword.nix {};
  posix-array = callPackage ./posix-array.nix {};
}
