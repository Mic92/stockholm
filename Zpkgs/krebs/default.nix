{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
in

pkgs //
{
  github-hosts-sync = callPackage ./github-hosts-sync.nix {};
  github-known_hosts = callPackage ./github-known_hosts.nix {};
}
