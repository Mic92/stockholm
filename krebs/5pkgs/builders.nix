{ lib, pkgs, ... }:
with lib;
rec {
  execve = name: { filename, argv ? null, envp ? {}, destination ? "" }: let
  in writeC name { inherit destination; } ''
    #include <unistd.h>

    static char *const filename = ${toC filename};

    ${if argv == null
      then /* Propagate arguments */ ''
        #define MAIN_ARGS int argc, char **argv
      ''
      else /* Provide fixed arguments */ ''
        #define MAIN_ARGS void
        static char *const argv[] = ${toC (argv ++ [null])};
      ''}

    static char *const envp[] = ${toC (
      mapAttrsToList (k: v: "${k}=${v}") envp ++ [null]
    )};

    int main (MAIN_ARGS) {
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

  writeDash = name: text: pkgs.writeScript name ''
    #! ${pkgs.dash}/bin/dash
    ${text}
  '';

  writeDashBin = name: text: pkgs.writeTextFile {
    executable = true;
    destination = "/bin/${name}";
    name = name;
    text = ''
      #! ${pkgs.dash}/bin/dash
      ${text}
    '';
  };

  writeNixFromCabal = name: path: pkgs.runCommand name {} ''
    ${pkgs.cabal2nix}/bin/cabal2nix ${path} > $out
  '';
}
