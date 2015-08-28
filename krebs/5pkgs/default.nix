{ lib, pkgs, ... }:

with import ../4lib { inherit lib; };

let
  inherit (pkgs) callPackage;
in

pkgs //
rec {
  cac = callPackage ./cac.nix {};
  dic = callPackage ./dic.nix {};
  genid = callPackage ./genid.nix {};
  github-hosts-sync = callPackage ./github-hosts-sync.nix {};
  github-known_hosts = callPackage ./github-known_hosts.nix {};
  hashPassword = callPackage ./hashPassword.nix {};
  nq = callPackage ./nq.nix {};
  posix-array = callPackage ./posix-array.nix {};

  execve = name: { filename, argv, envp }:
    writeC name {} ''
      #include <unistd.h>
      int main () {
        const char *filename = ${toC filename};
        char *const argv[] = ${toC (argv ++ [null])};
        char *const envp[] = ${toC (
          mapAttrsToList (k: v: "${k}=${v}") envp ++ [null]
        )};
        execve(filename, argv, envp);
        return -1;
      }
    '';

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
