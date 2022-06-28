{ name }: let
  inherit (import ../krebs/krops.nix { inherit name; })
    krebs-source
    lib
    pkgs
  ;


  source = { test }: lib.evalSource ([
    (krebs-source { test = test; })
    {
      nixos-config.symlink = "stockholm/lass/1systems/${name}/physical.nix";
      nixpkgs-unstable.git = {
        url = "https://github.com/nixos/nixpkgs";
        ref = (lib.importJSON ../krebs/nixpkgs-unstable.json).rev;
        shallow = true;
      };
      secrets = if test then {
        file = toString ./2configs/tests/dummy-secrets;
      } else {
        pass = {
          dir = "${lib.getEnv "HOME"}/sync/pwstore";
          name = "hosts/${name}";
        };
      };
      stockholm.file = lib.mkForce {
        path = toString ../.;
        useChecksum = true;
      };
    }
    (if lib.pathExists (./. + "/1systems/${name}/source.nix") then
      import (./. + "/1systems/${name}/source.nix") { inherit lib pkgs test; }
    else
      {}
    )
  ]);

in {

  deploy = { target ? "root@${name}/var/src", offline ? false }: pkgs.krops.writeCommand "deploy" {
    command = targetPath: ''

      set -xfu

      outDir=$(mktemp -d)
      trap "rm -rf $outDir;" INT TERM EXIT

      build=$(command -v nom-build || echo "nix-build")

      $build \
        -I "${targetPath}" \
        '<nixpkgs/nixos>' -A config.system.build.toplevel \
        -o "$outDir/out" \
        ${lib.optionalString offline "--option substitute false"} \
        # -vvvvv --show-trace

      nix-env -p /nix/var/nix/profiles/system --set "$outDir/out"

      "$outDir/out/bin/switch-to-configuration" switch
    '';
    source = source { test = false; };
    allocateTTY = true;
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
