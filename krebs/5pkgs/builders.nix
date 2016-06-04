{ config, pkgs, ... }:
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

  writeHaskell =
    k:
    let
      k' = parseDrvName k;
      name = k'.name;
      version = if k'.version != "" then k'.version else "0";
    in
    { base-depends ? ["base"]
    , executables ? {}
    , ghc-options ? ["-Wall" "-O3" "-threaded" "-rtsopts"]
    , haskellPackages ? pkgs.haskellPackages
    , library ? null
    , license ? "WTFPL"
    }:
    let
      isExecutable = executables != {};
      isLibrary = library != null;

      cabal-file = pkgs.writeText "${name}-${version}.cabal" ''
        build-type: Simple
        cabal-version: >= 1.2
        name: ${name}
        version: ${version}
        ${concatStringsSep "\n" (mapAttrsToList exe-section executables)}
        ${optionalString isLibrary (lib-section library)}
      '';

      exe-install =
        exe-name:
        { file ? pkgs.writeText "${name}-${exe-name}.hs" text
        , relpath ? "${exe-name}.hs"
        , text
        , ... }:
        if types.filename.check exe-name
          then "install -D ${file} $out/${relpath}"
          else throw "argument ‘exe-name’ is not a ${types.filename.name}";

      exe-section =
        exe-name:
        { build-depends ? base-depends ++ extra-depends
        , extra-depends ? []
        , file ? pkgs.writeText "${name}-${exe-name}.hs" text
        , relpath ? "${exe-name}.hs"
        , text
        , ... }: ''
          executable ${exe-name}
            build-depends: ${concatStringsSep "," build-depends}
            ghc-options: ${toString ghc-options}
            main-is: ${relpath}
        '';

      get-depends =
        { build-depends ? base-depends ++ extra-depends
        , extra-depends ? []
        , ...
        }:
        build-depends;

      lib-install =
        { exposed-modules
        , ... }:
        concatStringsSep "\n" (mapAttrsToList mod-install exposed-modules);

      lib-section =
        { build-depends ? base-depends ++ extra-depends
        , extra-depends ? []
        , exposed-modules
        , ... }: ''
          library
            build-depends: ${concatStringsSep "," build-depends}
            ghc-options: ${toString ghc-options}
            exposed-modules: ${concatStringsSep "," (attrNames exposed-modules)}
        '';

      mod-install =
        mod-name:
        { file ? pkgs.writeText "${name}-${mod-name}.hs" text
        , relpath ? "${replaceStrings ["."] ["/"] mod-name}.hs"
        , text
        , ... }:
        if types.haskell.modid.check mod-name
          then "install -D ${file} $out/${relpath}"
          else throw "argument ‘mod-name’ is not a ${types.haskell.modid.name}";
    in
      haskellPackages.mkDerivation {
        inherit isExecutable isLibrary license version;
        executableHaskellDepends =
          attrVals
            (concatMap get-depends (attrValues executables))
            haskellPackages;
        libraryHaskellDepends =
          attrVals
            (optionals isLibrary (get-depends library))
            haskellPackages;
        pname = name;
        src = pkgs.runCommand "${name}-${version}-src" {} ''
          install -D ${cabal-file} $out/${cabal-file.name}
          ${optionalString isLibrary (lib-install library)}
          ${concatStringsSep "\n" (mapAttrsToList exe-install executables)}
        '';
      };

  writeNixFromCabal =
    trace (toString [
      "The function `writeNixFromCabal` has been deprecated in favour of"
      "`writeHaskell`."
    ])
    (name: path: pkgs.runCommand name {} ''
      ${pkgs.cabal2nix}/bin/cabal2nix ${path} > $out
    '');
}
