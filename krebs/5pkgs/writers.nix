{ pkgs, ... }:
with import <stockholm/lib>;
{
  nixpkgs.config.packageOverrides = _: {

    # Combine a list of derivations using symlinks.  Paths in later derivations
    # take precedence over earlier ones.
    #
    # Example: create wrapper but retain all other files (man pages etc.)
    #
    # {
    #   nixpkgs.config.packageOverrides = super: {
    #     hello = pkgs.concat "hello" [
    #       super.hello
    #       (pkgs.writeDashBin "hello" ''
    #         echo OMG
    #         echo exec ${super.hello}/bin/hello "$@"
    #       '')
    #     ];
    #   };
    # }
    #
    concat = name: xs: pkgs.runCommand name {} ''
      mkdir $out
      ${flip concatMapStrings xs (x: ''
        cp --remove-destination -vrs ${x}/* $out
        find $out -type d -exec chmod -v u+rwx {} +
      '')}
    '';

    execve = name: { filename, argv ? null, envp ? {}, destination ? "" }: let
    in pkgs.writeC name { inherit destination; } /* c */ ''
      #include <unistd.h>

      static char *const filename = ${toC filename};

      ${if argv == null
        then /* Propagate arguments */ /* c */ ''
          #define MAIN_ARGS int argc, char **argv
        ''
        else /* Provide fixed arguments */ /* c */ ''
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

    execveBin = name: cfg:
      pkgs.execve name (cfg // { destination = "/bin/${name}"; });

    makeScriptWriter = interpreter: name: text:
      assert (with types; either absolute-pathname filename).check name;
      pkgs.writeOut (baseNameOf name) {
        ${optionalString (types.absolute-pathname.check name) name} = {
          executable = true;
          text = "#! ${interpreter}\n${text}";
        };
      };

    writeBash = name: text:
      assert (with types; either absolute-pathname filename).check name;
      pkgs.writeOut (baseNameOf name) {
        ${optionalString (types.absolute-pathname.check name) name} = {
          check = pkgs.writeDash "shellcheck.sh" ''
            ${pkgs.haskellPackages.ShellCheck}/bin/shellcheck "$1" || :
          '';
          executable = true;
          text = "#! ${pkgs.bash}/bin/bash\n${text}";
        };
      };

    writeBashBin = name:
      assert types.filename.check name;
      pkgs.writeBash "/bin/${name}";

    writeC = name: { destination ? "" }: text: pkgs.runCommand name {
      inherit text;
      passAsFile = [ "text" ];
    } /* sh */ ''
      PATH=${makeBinPath (with pkgs; [
        binutils
        coreutils
        gcc
      ])}
      exe=$out${destination}
      mkdir -p "$(dirname "$exe")"
      gcc -O -Wall -o "$exe" -x c "$textPath"
      strip --strip-unneeded "$exe"
    '';

    writeDash = pkgs.makeScriptWriter "${pkgs.dash}/bin/dash";

    writeDashBin = name:
      assert types.filename.check name;
      pkgs.writeDash "/bin/${name}";

    writeEximConfig = name: text: pkgs.runCommand name {
      inherit text;
      passAsFile = [ "text" ];
    } /* sh */ ''
      # TODO validate exim config even with config.nix.useChroot == true
      # currently doing so will fail because "user exim was not found"
      #${pkgs.exim}/bin/exim -C "$textPath" -bV >/dev/null
      mv "$textPath" $out
    '';

    writeOut = name: specs0:
    let
      writers.link =
        { path
        , link
        }:
        assert path == "" || types.absolute-pathname.check path;
        assert types.package.check link;
        {
          install = /* sh */ ''
            ${optionalString (dirOf path != "/") /* sh */ ''
              ${pkgs.coreutils}/bin/mkdir -p $out${dirOf path}
            ''}
            ${pkgs.coreutils}/bin/ln -s ${link} $out${path}
          '';
        };

      writers.text =
        { path
        , check ? null
        , executable ? false
        , mode ? if executable then "0755" else "0644"
        , text
        }:
        assert path == "" || types.absolute-pathname.check path;
        assert types.bool.check executable;
        assert types.file-mode.check mode;
        rec {
          var = "file_${hashString "sha1" path}";
          val = text;
          install = /* sh */ ''
            ${optionalString (check != null) /* sh */ ''
              ${check} ''$${var}Path
            ''}
            ${pkgs.coreutils}/bin/install \
                -m ${mode} \
                -D \
                ''$${var}Path $out${path}
          '';
        };

      write = spec: writers.${spec.type} (removeAttrs spec ["type"]);

      specs =
        mapAttrsToList
          (path: spec: let
            known-types = [ "link" "text" ];
            found-types = attrNames (getAttrs known-types spec);
            type = assert length found-types == 1; head found-types;
          in spec // { inherit path type; })
          specs0;

      files = map write specs;

      filevars = genAttrs' (filter (hasAttr "var") files)
                           (spec: nameValuePair spec.var spec.val);

      env = filevars // { passAsFile = attrNames filevars; };
    in
      pkgs.runCommand name env /* sh */ ''
        set -efu
        ${concatMapStringsSep "\n" (getAttr "install") files}
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

        cabal-file = pkgs.writeText "${name}-${version}.cabal" /* cabal */ ''
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
            then /* sh */ "install -D ${file} $out/${relpath}"
            else throw "argument ‘exe-name’ is not a ${types.filename.name}";

        exe-section =
          exe-name:
          { build-depends ? base-depends ++ extra-depends
          , extra-depends ? []
          , file ? pkgs.writeText "${name}-${exe-name}.hs" text
          , relpath ? "${exe-name}.hs"
          , text
          , ... }: /* cabal */ ''
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
          , ... }: /* cabal */ ''
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
            then /* sh */ "install -D ${file} $out/${relpath}"
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
          src = pkgs.runCommand "${name}-${version}-src" {} /* sh */ ''
            install -D ${cabal-file} $out/${cabal-file.name}
            ${optionalString isLibrary (lib-install library)}
            ${concatStringsSep "\n" (mapAttrsToList exe-install executables)}
          '';
        };

    writeJq = name: text:
      assert (with types; either absolute-pathname filename).check name;
      pkgs.writeOut (baseNameOf name) {
        ${optionalString (types.absolute-pathname.check name) name} = {
          check = pkgs.writeDash "jqcheck.sh" ''
            exec ${pkgs.jq}/bin/jq -f "$1" < /dev/null
          '';
          inherit text;
        };
      };

    writeJSON = name: value: pkgs.writeText name (toJSON value);

    writeNixFromCabal =
      trace (toString [
        "The function `writeNixFromCabal` has been deprecated in favour of"
        "`writeHaskell`."
      ])
      (name: path: pkgs.runCommand name {} /* sh */ ''
        ${pkgs.cabal2nix}/bin/cabal2nix ${path} > $out
      '');

    writePython2 = name: text:
      assert (with types; either absolute-pathname filename).check name;
      pkgs.writeOut (baseNameOf name) {
        ${optionalString (types.absolute-pathname.check name) name} = {
          check = pkgs.writeDash "python2check.sh" ''
            exec ${pkgs.python2}/bin/python -m py_compile "$1"
          '';
          inherit text;
        };
      };

    writePython3 = name: text:
      assert (with types; either absolute-pathname filename).check name;
      pkgs.writeOut (baseNameOf name) {
        ${optionalString (types.absolute-pathname.check name) name} = {
          check = pkgs.writeDash "python3check.sh" ''
            exec ${pkgs.python3}/bin/python -m py_compile "$textPath"
          '';
          inherit text;
        };
      };

    writeSed = pkgs.makeScriptWriter "${pkgs.gnused}/bin/sed -f";
  };
}
