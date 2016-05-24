{ config, lib, pkgs, ... }:
with config.krebs.lib;
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
    PATH=${makeBinPath (with pkgs; [
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

  writeEximConfig = name: text: pkgs.runCommand name {
    inherit text;
    passAsFile = [ "text" ];
  } ''
    # TODO validate exim config even with config.nix.useChroot == true
    # currently doing so will fail because "user exim was not found"
    #${pkgs.exim}/bin/exim -C "$textPath" -bV >/dev/null
    mv "$textPath" $out
  '';

  writeHaskellBin =
    k:
    let
      k' = parseDrvName k;
      name = k'.name;
      version = if k'.version != "" then k'.version else "0";
    in
    { build-depends ? ["base"] ++ depends
    , depends ? []
    , ghc-options ? ["-Wall" "-O3" "-threaded" "-rtsopts"]
    , haskellPackages ? pkgs.haskellPackages
    , license ? "WTFPL"
    }:
    main-text:
    let
      cabal-file = pkgs.writeText "${name}-${version}.cabal" ''
        build-type: Simple
        cabal-version: >= 1.2
        name: ${name}
        version: ${version}

        executable ${name}
          build-depends: ${concatStringsSep "," build-depends}
          ghc-options: ${toString ghc-options}
          main-is: ${main-file.name}
      '';
      main-file = pkgs.writeText "${name}-${version}.hs" main-text;
    in
      haskellPackages.mkDerivation rec {
        inherit license version;
        executableHaskellDepends = attrVals build-depends haskellPackages;
        isExecutable = true;
        isLibrary = false;
        pname = name;
        src = pkgs.runCommand "${name}-${version}-src" {} ''
          install -D ${cabal-file} $out/${cabal-file.name}
          install -D ${main-file}  $out/${main-file.name}
        '';
      };

  writeNixFromCabal = name: path: pkgs.runCommand name {} ''
    ${pkgs.cabal2nix}/bin/cabal2nix ${path} > $out
  '';
}
