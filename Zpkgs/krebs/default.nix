{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
in

pkgs //
{
  genid = callPackage ./genid.nix {};
  github-hosts-sync = callPackage ./github-hosts-sync.nix {};
  github-known_hosts = callPackage ./github-known_hosts.nix {};
  hashPassword = callPackage ./hashPassword.nix {};
}
