{ name }: let
  inherit (import ../krebs/krops.nix { inherit name; })
    krebs-source
    lib
    pkgs
  ;

  source = { test }: lib.evalSource [
    (krebs-source { test = test; })
    {
      nixos-config.symlink = "stockholm/jeschli/1systems/${name}/config.nix";
      nixpkgs-unstable.git = {
        url = "https://github.com/nixos/nixpkgs";
        ref = (lib.importJSON ../krebs/nixpkgs-unstable.json).rev;
      };
      secrets = if test then {
        file = toString ./2configs/tests/dummy-secrets;
      } else {
        file = "${lib.getEnv "HOME"}/secrets/${name}";
      };
    }
    {
       home-manager.git = {
         url = https://github.com/rycee/home-manager;
         ref = "2ccbf43";
       };
    }
  ];

in {
  # usage: $(nix-build --no-out-link --argstr name HOSTNAME -A deploy)
  deploy = { target ? "root@${name}/var/src" }: pkgs.krops.writeDeploy "${name}-deploy" {
    source = source { test = false; };
    inherit target;
  };

  # usage: $(nix-build --no-out-link --argstr name HOSTNAME --argstr target PATH -A test)
  test = { target }: pkgs.krops.writeTest "${name}-test" {
    force = true;
    inherit target;
    source = source { test = true; };
  };
}
