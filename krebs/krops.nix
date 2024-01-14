{ name, target ? name }: rec {

  krops = ../submodules/krops;

  lib = import "${krops}/lib";

  # TODO document why pkgs should be used like this
  pkgs = import "${krops}/pkgs" {};

  krebs-source = { test ? false }: rec {
    nixpkgs = if test then {
      derivation = let
        rev = (lib.importJSON ../flake.lock).nodes.nixpkgs.locked.rev;
        sha256 = (lib.importJSON ../flake.lock).nodes.nixpkgs.locked.narHash;
      in ''
        with import (builtins.fetchTarball {
          url = "https://github.com/nixos/nixpkgs/archive/${rev}.tar.gz";
          sha256 = "${sha256}";
        }) {};
        pkgs.fetchFromGitHub {
          owner = "nixos";
          repo = "nixpkgs";
          rev = "${rev}";
          sha256 = "${sha256}";
        }
      '';
    } else {
      git = {
        ref = (lib.importJSON ../flake.lock).nodes.nixpkgs.locked.rev;
        url = "https://github.com/NixOS/nixpkgs";
        shallow = true;
      };
    };
    stockholm.file = toString ../.;
    stockholm-version.pipe =
      toString (pkgs.writers.writeDash "${name}-version" ''
        set -efu
        cd ${lib.escapeShellArg stockholm.file}
        V=$(${pkgs.coreutils}/bin/date +%y.%m)
        if test -d .git; then
          V=$V.git.$(${pkgs.git}/bin/git describe --always --dirty)
          case $V in (*-dirty)
            V=$V@''${HOSTNAME-$(${pkgs.nettools}/bin/hostname)}
          esac
        fi
        printf %s "$V"
      '');
  };

  source ={ test }: lib.evalSource [
    (krebs-source { test = test; })
    {
      nixos-config.symlink = "stockholm/krebs/1systems/${name}/config.nix";
      secrets = if test then {
        file = toString ./0tests/data/secrets;
      } else {
        pass = {
          dir = "${lib.getEnv "HOME"}/brain";
          name = "krebs-secrets/${name}";
        };
      };
    }
  ];

  # usage: $(nix-build --no-out-link --argstr name HOSTNAME -A deploy)
  deploy = pkgs.krops.writeDeploy "${name}-deploy" {
    source = source { test = false; };
    target = "root@${target}/var/src";
  };

  # usage: $(nix-build --no-out-link --argstr name HOSTNAME --argstr target PATH -A populate)
  populate = { target, force ? false }: pkgs.populate {
    inherit force;
    source = source { test = false; };
    target = lib.mkTarget target;
  };

  # usage: $(nix-build --no-out-link --argstr name HOSTNAME --argstr target PATH -A test)
  test = { target }: pkgs.krops.writeTest "${name}-test" {
    force = true;
    inherit target;
    source = source { test = true; };
  };
}
