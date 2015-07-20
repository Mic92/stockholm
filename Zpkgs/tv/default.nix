{ pkgs, ... }:

let
  inherit (pkgs) callPackage;
in

pkgs //
{
  charybdis = callPackage ./charybdis {};
  dic = callPackage ./dic.nix {};
  genid = callPackage ./genid.nix {};
  github-hosts-sync = callPackage ./github-hosts-sync.nix {};
  github-known_hosts = callPackage ./github-known_hosts.nix {};
  lentil = callPackage ./lentil {};
  much = callPackage ./much.nix {};
  viljetic-pages = callPackage ./viljetic-pages {};
}
