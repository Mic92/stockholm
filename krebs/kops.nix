{ name }: rec {

  kops = (import <nixpkgs> {}).fetchgit {
    url = https://cgit.krebsco.de/kops/;
    rev = "e89cf20d4310070a877c2e24a287659546b561c9";
    sha256 = "0wg8d80sxa46z4i7ir79sci2hwmv3qskzqdg0si64p6vazy8vckb";
  };

  lib = import "${kops}/lib";

  # TODO document why pkgs should be used like this
  pkgs = import "${kops}/pkgs" {};

  krebs-source = {
    nixpkgs.git = {
      ref = "b50443b5c4ac0f382c49352a892b9d5d970eb4e7";
      url = https://github.com/NixOS/nixpkgs;
    };
    stockholm.file = toString ../.;
    stockholm-version.pipe = toString (pkgs.writeDash "${name}-version" ''
      set -efu
      cd $HOME/stockholm
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
  deploy = pkgs.kops.writeDeploy "${name}-deploy" {
    source = source { test = false; };
    target = "root@${name}/var/src";
  };

  # usage: $(nix-build --no-out-link --argstr name HOSTNAME -A test)
  test = pkgs.kops.writeTest "${name}-test" {
    source = source { test = true; };
    target = "${lib.getEnv "HOME"}/tmp/${name}-kops-test-src";
  };
}
