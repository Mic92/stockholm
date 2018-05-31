{ name }: let
  inherit (import ../krebs/kops.nix { inherit name; })
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
