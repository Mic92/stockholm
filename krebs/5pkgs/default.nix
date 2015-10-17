{ pkgs, ... }:

with import ../4lib { inherit (pkgs) lib; };

let
  subdirs = mapAttrs (_: flip pkgs.callPackage {}) (subdirsOf ./.);
  pkgs' = pkgs // subdirs;
in

subdirs // rec {

  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super:
      mapAttrs (name: path: self.callPackage path {})
        (mapAttrs'
          (name: type:
            if hasSuffix ".nix" name
              then {
                name = removeSuffix ".nix" name;
                value = ./haskell-overrides + "/${name}";
              }
              else null)
          (builtins.readDir ./haskell-overrides));
  };

  push = pkgs'.callPackage ./push {
    inherit (subdirs) get jq;
  };

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
    PATH=${makeSearchPath "bin" (with pkgs; [
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
