{ config ? config, name }: let
  inherit (import ../krebs/krops.nix { inherit name; })
    krebs-source
    lib
    pkgs
  ;

  source = { test }: lib.evalSource [
    krebs-source
    {
      nixos-config.symlink = "stockholm/lass/1systems/${name}/physical.nix";
      secrets = if test then {
        file = toString ./2configs/tests/dummy-secrets;
      } else {
        pass = {
          dir = "${lib.getEnv "HOME"}/.password-store";
          name = "hosts/${name}";
        };
      };
    }
  ];

in {
  # usage: $(nix-build --no-out-link --argstr name HOSTNAME -A deploy)
  deploy = pkgs.krops.writeDeploy "${name}-deploy" {
    source = source { test = false; };
    target = "root@${name}/var/src";
  };

  # usage: $(nix-build --no-out-link --argstr name HOSTNAME -A test)
  test = pkgs.krops.writeTest "${name}-test" {
    source = source { test = true; };
    target = "${lib.getEnv "HOME"}/tmp/${name}-krops-test-src";
  };

  ci = map (host:
    pkgs.krops.writeTest "${host.name}-test" {
      source = source { test = true; };
      target = "${lib.getEnv "TMPDIR"}/lass/${host.name}";
    }
  ) (lib.filter (host: lib.getAttr "ci" host && host.owner == "lass") (lib.attrValues config.krebs.hosts));
}
