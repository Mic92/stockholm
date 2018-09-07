{ config ? config, name, target}: let
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
        file = "/home/lass/stockholm/lass/2configs/tests/dummy-secrets";
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
    inherit target;
  };

  # usage: $(nix-build --no-out-link --argstr name HOSTNAME -A test)
  ci = pkgs.krops.writeTest "${name}-test" {
    source = source { test = true; };
    inherit target;
  };
}
