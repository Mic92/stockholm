{ config, pkgs, ... }:

let
  inherit (pkgs) lib stdenv;
  inherit (lib.strings) concatStringsSep stringAsChars;
  inherit (lib.attrsets) attrValues mapAttrs;
  inherit (lib) makeSearchPath;
  inherit (import ../../lib { inherit pkgs; }) shell-escape;


  # TODO make these scripts available in an maintenance shell


  scripts = {

    # just so we don't reboot accidentally
    reboot =
      ''
        echo no reboot >&2
        exit 23
      '';

    rebuild =
      ''
        nixpkgs=''${nixpkgs-/home/tv/src/nixpkgs}
        nixos-rebuild \
            --show-trace \
            -I nixpkgs="$nixpkgs" \
            switch \
            2>&1 \
          | sed ${shell-escape ''
              s|"\(/home/tv/src/config/[^":]*\)"|[31;1m\1[m|
              s|^trace:\s*\(.*\)|[35;1m\1[m|
          ''}
      '';

  };

  wrap = script:
    ''
      #! /bin/sh
      set -euf
      ${script}
    '';
      #lib=$lib
      #export PATH=$bin:${makeSearchPath "bin" buildInputs}

  buildScript = name: script:
    builtins.trace "building ${name}"
    ''
      echo ${shell-escape script} > $bin/${shell-escape name}
      chmod +x $bin/${shell-escape name}
    '';



  tools = pkgs.stdenv.mkDerivation rec {
    name = "tools";
    src = /var/empty;

    buildInputs = [];


    buildPhase =
      ''
        mkdir $out

        bin=$out/bin
        mkdir $bin

        ${concatStringsSep "\n" (attrValues (mapAttrs buildScript scripts))}

      '';
      #''
      #mkdir $out

      #lib=$out/lib
      #cp -r lib $lib

      #bin=$out/bin
      #mkdir $bin
      #${concatStringsSep "\n" (attrValues (mapAttrs (name: script:
      #    ''
      #    {
      #      echo '#! /bin/sh'
      #      echo 'set -euf'
      #      echo "lib=$lib"
      #      echo "export PATH=$bin:${makeSearchPath "bin" buildInputs}"
      #      echo ${shell-escape script}
      #    } > $bin/${name}
      #    chmod +x $bin/${name}
      #    '') scripts))}
      #'';
    installPhase = ":";
  };

in

{
  environment.systemPackages = [ tools ];
}
