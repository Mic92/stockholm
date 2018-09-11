{ name }: rec {

  krops = builtins.fetchGit {
    url = https://cgit.krebsco.de/krops/;
    rev = "4e466eaf05861b47365c5ef46a31a188b70f3615";
  };

  lib = import "${krops}/lib";

  # TODO document why pkgs should be used like this
  pkgs = import "${krops}/pkgs" {};

  krebs-source = {
    nixpkgs.git = {
      ref = (lib.importJSON ./nixpkgs.json).rev;
      url = https://github.com/NixOS/nixpkgs;
    };
    stockholm.file = toString ../.;
    stockholm-version.pipe = toString (pkgs.writeDash "${name}-version" ''
      set -efu
      cd ${lib.escapeShellArg krebs-source.stockholm.file}
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
    krebs-source
    {
      nixos-config.symlink = "stockholm/krebs/1systems/${name}/config.nix";
      secrets =
        if test
          then {
            file = toString <stockholm/krebs/0tests/data/secrets>;
          }
          else {
            pass = {
              dir = "${lib.getEnv "HOME"}/brain";
              name = "krebs-secrets/${name}";
            };
          }
        ;
    }
  ];

  # usage: $(nix-build --no-out-link --argstr name HOSTNAME -A deploy)
  deploy = pkgs.krops.writeDeploy "${name}-deploy" {
    source = source { test = false; };
    target = "root@${name}/var/src";
  };

  # usage: $(nix-build --no-out-link --argstr name HOSTNAME --argstr target PATH -A test)
  test = { target }: pkgs.krops.writeTest "${name}-test" {
    inherit target;
    source = source { test = true; };
  };
}
