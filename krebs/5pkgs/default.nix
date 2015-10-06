{ lib, pkgs, ... }:

with import ../4lib { inherit lib; };

let
  inherit (pkgs) callPackage;
in

rec {
  cac = callPackage ./cac {};
  charybdis = callPackage ./charybdis {};
  dic = callPackage ./dic {};
  genid = callPackage ./genid {};
  get = callPackage ./get {};
  github-hosts-sync = callPackage ./github-hosts-sync {};
  github-known_hosts = callPackage ./github-known_hosts {};
  hashPassword = callPackage ./hashPassword {};
  jq = callPackage ./jq {};
  krebszones = callPackage ./krebszones {};
  lentil = callPackage ./lentil {};
  much = callPackage ./much {};
  nq = callPackage ./nq {};
  posix-array = callPackage ./posix-array {};
  pssh = callPackage ./pssh {};
  passwdqc-utils = callPackage ./passwdqc-utils {};
  Reaktor = callPackage ./Reaktor {};
  youtube-tools = callPackage ./youtube-tools {};

  execve = name: { filename, argv, envp ? {}, destination ? "" }:
    writeC name { inherit destination; } ''
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

  execveBin = name: cfg: execve name (cfg // { destination = "/bin/${name}"; });

  writeC = name: { destination ? "" }: src: pkgs.runCommand name {} ''
    PATH=${lib.makeSearchPath "bin" (with pkgs; [
      binutils
      coreutils
      gcc
    ])}
    src=${pkgs.writeText "${name}.c" src}
    exe=$out${destination}
    mkdir -p "$(dirname "$exe")"
    gcc -O -Wall -o "$exe" $src
    strip --strip-unneeded "$exe"
  '';
}
