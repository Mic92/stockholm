{ config, lib, ... }:

with import ../../4lib { inherit lib; };

let
  target = config.krebs.build // { user.name = "root"; };

  out = {
    # TODO deprecate krebs.build.host
    options.krebs.build.host = mkOption {
      type = types.host;
    };

    # TODO make krebs.build.profile shell safe
    options.krebs.build.profile = mkOption {
      type = types.str;
      default = "/nix/var/nix/profiles/system";
    };

    # TODO make krebs.build.target.host :: host
    options.krebs.build.target = mkOption {
      type = with types; nullOr str;
      default = null;
    };

    # TODO deprecate krebs.build.user
    options.krebs.build.user = mkOption {
      type = types.user;
    };

    options.krebs.build.scripts.deploy = lib.mkOption {
      type = lib.types.str;
      default = ''
        set -efu
        (${config.krebs.build.scripts._source})
        ${ssh-target ''
          ${config.krebs.build.scripts._nix-env}
          ${config.krebs.build.profile}/bin/switch-to-configuration switch
        ''}
        echo OK
      '';
    };

    options.krebs.build.scripts.infest = lib.mkOption {
      type = lib.types.str;
      default = ''
        set -efu

        export RSYNC_RSH; RSYNC_RSH="$(type -p ssh) \
          -o 'HostName ${target.host.infest.addr}' \
          -o 'Port ${toString target.host.infest.port}' \
        "
        ssh() {
          eval "$RSYNC_RSH \"\$@\""
        }

        ${ssh-target ''
          ${readFile ./infest/prepare.sh}
          ${readFile ./infest/install-nix.sh}
        ''}

        (${config.krebs.build.scripts._source})

        ${ssh-target ''
          export PATH; PATH=/root/.nix-profile/bin:$PATH

          src=$(type -p nixos-install)
          cat_src() {
            sed < "$src" "$(
              sed < "$src" -n '
                  /^if ! test -e "\$mountPoint\/\$NIXOS_CONFIG/,/^fi$/=
                  /^nixpkgs=/=
                  /^NIX_PATH=/,/^$/{/./=}
                ' \
                | sed 's:$:s/^/#krebs#/:'
            )"
          }

          # Location to insert config.krebs.build.scripts._nix-env
          i=$(sed -n '/^echo "building the system configuration/=' "$src")

          {
            cat_src | sed -n "1,$i{p}"
            cat ${doc config.krebs.build.scripts._nix-env}
            cat_src | sed -n "$i,\''${$i!p}"
          } > nixos-install
          chmod +x nixos-install

          # Wrap inserted config.krebs.build.scripts._nix-env into chroot.
          nix_env=$(cat_src | sed -n '
            s:.*\(/nix/store/[a-z0-9]*-nix-[0-9.]\+/bin/nix-env\).*:\1:p;T;q
          ')
          echo nix-env is $nix_env
          sed -i '
            s:^nix-env:chroot $mountPoint '"$nix_env"':
          ' nixos-install

          ./nixos-install

          ${readFile ./infest/finalize.sh}
        ''}
      '';
    };

    options.krebs.build.scripts._nix-env = lib.mkOption {
      type = lib.types.str;
      default = ''
        set -efu
        NIX_PATH=${config.krebs.build.source.NIX_PATH} \
        nix-env \
          -f '<stockholm>' \
          -Q \
          --argstr user-name ${config.krebs.exec.user.name} \
          --argstr host-name ${target.host.name} \
          --profile ${config.krebs.build.profile} \
          --set \
          -A ${lib.escapeShellArg (lib.concatStringsSep "." [
                config.krebs.build.user.name
                config.krebs.build.host.name
                "system"
              ])}
      '';
    };

    options.krebs.build.scripts._source = lib.mkOption {
      type = lib.types.str;
      default = ''
        set -efu
        ${
          lib.concatStringsSep "\n"
            (lib.mapAttrsToList
              (name: { scripts, url, ... }: "(${scripts._source})")
              (config.krebs.build.source.dir //
               config.krebs.build.source.git))
        }
      '';
    };

    options.krebs.build.source.NIX_PATH = mkOption {
      type = types.str;
      default =
        lib.concatStringsSep ":"
          (lib.mapAttrsToList (name: _: "${name}=/root/${name}")
            (config.krebs.build.source.dir //
             config.krebs.build.source.git));
    };

    options.krebs.build.source.dir = mkOption {
      type =
        let
          exec = config.krebs.exec;
        in
        types.attrsOf (types.submodule ({ config, ... }:
          let
            url = "file://${config.host.name}${config.path}";

            can-link = config.host.name == target.host.name;
            can-push = config.host.name == exec.host.name;

            push-method = ''
              rsync \
                --exclude .git \
                --exclude .graveyard \
                --exclude old \
                --exclude tmp \
                --rsync-path='mkdir -p ${config.target-path} && rsync' \
                --delete-excluded \
                -vrLptgoD \
                ${config.path}/ \
                ${target.user.name}@${target.host.name}:${config.target-path}
            '';
          in
          {
            options = {
              host = mkOption {
                type = types.host;
                description = ''
                define the host where the directory is stored on.
                XXX: currently it is just used to check if rsync is working,
                     becomes part of url
                '';
              };
              path = mkOption {
                type = types.str;
              };
              scripts._source = mkOption {
                type = types.str;
                default =
                  #if can-link then link-method else
                  if can-push then push-method else
                  throw "cannot source ${url}";
              };
              target-path = mkOption {
                type = types.str;
                default = "/root/${config._module.args.name}";
              };
              url = mkOption {
                type = types.str;
                default = "file://${config.host.name}${config.path}";
              };
            };
          }
        ));
      default = {};
    };

    options.krebs.build.source.git = mkOption {
      type =
        let
          target = config.krebs.build // { user.name = "root"; };
        in
        with types; attrsOf (submodule ({ config, ... }:
          {
            options = {
              url = mkOption {
                type = types.str; # TODO must be shell safe
              };
              rev = mkOption {
                type = types.str;
              };
              scripts._source = mkOption {
                type = types.str;
                default = ssh-target ''
                  mkdir -p ${config.target-path}
                  cd ${config.target-path}
                  if ! test -e .git; then
                    git init
                  fi
                  if ! cur_url=$(git config remote.origin.url 2>/dev/null); then
                    git remote add origin ${config.url}
                  elif test "$cur_url" != ${config.url}; then
                    git remote set-url origin ${config.url}
                  fi
                  if test "$(git rev-parse --verify HEAD 2>/dev/null)" != ${config.rev}; then
                    git fetch origin
                    git checkout ${config.rev} -- .
                    git checkout -q ${config.rev}
                    git submodule init
                    git submodule update
                  fi
                  git clean -dxf
                '';
              };
              target-path = mkOption {
                type = types.str;
                default = "/root/${config._module.args.name}";
              };
            };
          }
      ));
      default = {};
    };
  };

  doc = s:
    let b = "EOF${hashString "sha256" s}"; in
    ''
    <<\${b}
    ${s}
    ${b}
    '';

  ssh-target = script:
    "ssh root@${target.host.name} -T ${doc ''
      set -efu
      ${script}
    ''}";

in out
