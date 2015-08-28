{ lib, pkgs, ... }:

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
  nq = callPackage ./nq.nix {};
  posix-array = callPackage ./posix-array.nix {};

  writeC = name: {}: src: pkgs.runCommand name {} ''
    PATH=${lib.makeSearchPath "bin" (with pkgs; [
      binutils
      coreutils
      gcc
    ])}
    in=${pkgs.writeText "${name}.c" src}
    gcc -O -Wall -o $out $in
    strip --strip-unneeded $out
  '';
}
