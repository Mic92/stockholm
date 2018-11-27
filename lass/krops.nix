{ name }: let
  inherit (import ../krebs/krops.nix { inherit name; })
    krebs-nixpkgs
    krebs-source
    lib
    pkgs
  ;

  host-source = if lib.pathExists (./. + "/1systems/${name}/source.nix") then
    import (./. + "/1systems/${name}/source.nix") { inherit lib pkgs; }
  else
    {}
  ;

  source = { test }: lib.evalSource [
    (krebs-nixpkgs { test = test; })
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
    host-source
  ];

in {

  # usage: $(nix-build --no-out-link --argstr name HOSTNAME -A deploy)
  deploy = { target ? "root@${name}/var/src" }: pkgs.krops.writeDeploy "${name}-deploy" {
    source = source { test = false; };
    inherit target;
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
