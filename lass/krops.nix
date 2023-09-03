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
      nixpkgs = lib.mkForce (if test then {
        derivation = let
          rev = (lib.importJSON ../krebs/nixpkgs-unstable.json).rev;
          sha256 = (lib.importJSON ../krebs/nixpkgs-unstable.json).sha256;
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
          ref = (lib.importJSON ../krebs/nixpkgs-unstable.json).rev;
          url = https://github.com/NixOS/nixpkgs;
          shallow = true;
        };
      });
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

  deploy = { target ? "root@${name}/var/src", offline ? false, command ? "switch" }: pkgs.krops.writeCommand "deploy" {
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

      "$outDir/out/bin/switch-to-configuration" ${command}
    '';
    source = source { test = false; };
    allocateTTY = true;
    backup = false;
    inherit target;
  };

  deployWithFlake = { target ? "root@${name}/var/src", offline ? false }: pkgs.krops.writeCommand "deploy" {
    source = {
      inherit (source { test = false; }) stockholm secrets;
    };
    command = targetPath: ''
    '';
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

  deploy-with-diff = { target ? "root@${name}/var/src" }: pkgs.krops.writeCommand "${name}-deploy" {
    command = targetPath: ''
      set -xu
      deployScript=$(mktemp)
      cat << EOF > "$deployScript"
      #! /usr/bin/env nix-shell
      #! nix-shell -p nix-diff proot rsync -i bash
      set -xfu

      oldPath=\$(echo "${targetPath}" | sed 's/-new$//')
      oldSystemDrv=\$(nix show-derivation /run/current-system | jq -r 'keys[0]')
      newSystemDrv=\$(proot -b /var/src-new:/var/src nix-instantiate -I /var/src '<nixpkgs/nixos>' -A config.system.build.toplevel)

      (
        diff -rq -x '.git' "\$oldPath" "${targetPath}"
        nix-diff --color always --line-oriented "\$oldSystemDrv" "\$newSystemDrv"
      ) | less -R
      echo 'continue? [(Y)es]/(n)o'
      read yn
      case \$yn in
        [Nn]* ) exit;;
      esac
      rsync -ra --delete /var/src-new/ /var/src/
      nixos-rebuild -I /var/src switch
      EOF

      chmod +x "$deployScript"
      echo "$deployScript"
      cat "$deployScript"
      exec "$deployScript"
      rm "$deployScript"
    '';
    target = "${target}-new";
    source = source { test = false; };
    force = true;
    allocateTTY = true;
  };
}
