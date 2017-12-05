{ pkgs }: let

  stockholm-dir = ../../../..;

  lib = import (stockholm-dir + "/lib");

  #
  # high level commands
  #

  cmds.deploy = pkgs.withGetopt {
    diff = { default = /* sh */ "false"; switch = true; };
    force-populate = { default = /* sh */ "false"; switch = true; };
    quiet = { default = /* sh */ "false"; switch = true; };
    source_file = {
      default = /* sh */ "$user/1systems/$system/source.nix";
      long = "source";
    };
    system = {};
    target.default = /* sh */ "$system";
    user.default = /* sh */ "$LOGNAME";
  } (opts: pkgs.writeDash "stockholm.deploy" ''
    set -efu

    . ${init.env}
    . ${init.proxy "deploy" opts}

    if \test ${opts.diff.ref} = true; then

      system_profile=/nix/var/nix/profiles/system
      system_drv_cur=/etc/system.drv

      system_drv_new=$(
        ${pkgs.nix}/bin/nix-instantiate \
            -Q \
            -I "$target_path" \
            -E '
              (import <nixpkgs/nixos/lib/eval-config.nix> {
                modules = [ <nixos-config> ];
              }).config.system.build.toplevel
            '
      )

      if \test -e "$system_drv_cur"; then

        system_drv_cur_c=$(${pkgs.coreutils}/bin/readlink -f "$system_drv_cur")
        system_drv_new_c=$(${pkgs.coreutils}/bin/readlink -f "$system_drv_new")

        if \test "$system_drv_cur_c" = "$system_drv_new_c"; then
          echo "$0: system up to date" >&2
          exit 0
        fi

        system_drv_cur=$system_drv_cur_c \
        system_drv_new=$system_drv_new_c \
        ${pkgs.utillinux}/bin/script \
            --command '
              ${pkgs.haskellPackages.nix-diff}/bin/nix-diff \
                  "$system_drv_cur" "$system_drv_new"
            ' \
            --quiet \
            --return \
            /dev/null

        printf 'deploy? [N/y] ' >&2
        read -r REPLY
        if \test "$REPLY" != y; then
          echo "$0: abort!" >&2
          exit 1
        fi
      else
        echo "$0: --${opts.diff.long} has no effect because "$system_drv_cur" doesn't exist" >&2
      fi

      new_system=$(${pkgs.nix}/bin/nix-store --realize "$system_drv_new")

      ${pkgs.nix}/bin/nix-env -p "$system_profile" --set "$new_system"
      PATH=${lib.makeBinPath [
        pkgs.systemd
      ]} \
      "$system_profile"/bin/switch-to-configuration switch

      ${pkgs.coreutils}/bin/ln -fns "$system_drv_new" "$system_drv_cur"
      exit
    fi

    # Use system's nixos-rebuild, which is not self-contained
    export PATH=/run/current-system/sw/bin
    exec ${utils.with-whatsupnix} \
    nixos-rebuild switch \
        --show-trace \
        -I "$target_path"
  '');

  cmds.install = pkgs.withGetopt {
    force-populate = { default = /* sh */ "false"; switch = true; };
    quiet = { default = /* sh */ "false"; switch = true; };
    source_file = {
      default = /* sh */ "$user/1systems/$system/source.nix";
      long = "source";
    };
    system = {};
    target = {};
    user.default = /* sh */ "$LOGNAME";
  } (opts: pkgs.writeBash "stockholm.install" ''
    set -efu

    . ${init.env}

    if \test "''${using_proxy-}" != true; then
      ${pkgs.openssh}/bin/ssh \
          -o StrictHostKeyChecking=no \
          -o UserKnownHostsFile=/dev/null \
          "$target_user@$target_host" -p "$target_port" \
          env target_path=$(${pkgs.quote}/bin/quote "$target_path") \
              sh -s prepare \
            < ${stockholm-dir + "/krebs/4lib/infest/prepare.sh"}
              # TODO inline prepare.sh?
    fi

    . ${init.proxy "install" opts}

    # Reset PATH because we need access to nixos-install.
    # TODO provide nixos-install instead of relying on prepare.sh
    export PATH="$OLD_PATH"

    # these variables get defined by nix-shell (i.e. nix-build) from
    # XDG_RUNTIME_DIR and reference the wrong directory (/run/user/0),
    # which only exists on / and not at /mnt.
    export NIX_BUILD_TOP=/tmp
    export TEMPDIR=/tmp
    export TEMP=/tmp
    export TMPDIR=/tmp
    export TMP=/tmp
    export XDG_RUNTIME_DIR=/tmp

    export NIXOS_CONFIG="$target_path/nixos-config"

    cd
    exec nixos-install
  '');

  cmds.test = pkgs.withGetopt {
    force-populate = { default = /* sh */ "false"; switch = true; };
    quiet = { default = /* sh */ "false"; switch = true; };
    source_file = {
      default = /* sh */ "$user/1systems/$system/source.nix";
      long = "source";
    };
    system = {};
    target = {};
    user.default = /* sh */ "$LOGNAME";
  } (opts: pkgs.writeDash "stockholm.test" /* sh */ ''
    set -efu

    export dummy_secrets=true

    . ${init.env}
    . ${init.proxy "test" opts}

    exec ${utils.build} config.system.build.toplevel
  '');

  #
  # low level commands
  #

  # usage: get-source SOURCE_FILE
  cmds.get-source = pkgs.writeDash "stockholm.get-source" ''
    set -efu
    exec ${pkgs.nix}/bin/nix-instantiate \
        --eval \
        --json \
        --readonly-mode \
        --show-trace \
        --strict \
        "$1"
  '';

  # usage: parse-target [--default=TARGET] TARGET
  # TARGET = [USER@]HOST[:PORT][/PATH]
  cmds.parse-target = pkgs.withGetopt {
    default_target = {
      long = "default";
      short = "d";
    };
  } (opts: pkgs.writeDash "stockholm.parse-target" ''
    set -efu
    target=$1; shift
    for arg; do echo "$0: bad argument: $arg" >&2; done
    if \test $# != 0; then exit 2; fi
    exec ${pkgs.jq}/bin/jq \
        -enr \
        --arg default_target "$default_target" \
        --arg target "$target" \
        -f ${pkgs.writeText "stockholm.parse-target.jq" ''
          def parse: match("^(?:([^@]+)@)?([^:/]+)?(?::([0-9]+))?(/.*)?$") | {
            user: .captures[0].string,
            host: .captures[1].string,
            port: .captures[2].string,
            path: .captures[3].string,
          };
          def sanitize: with_entries(select(.value != null));
          ($default_target | parse) + ($target | parse | sanitize) |
          . + { local: (.user == env.LOGNAME and .host == env.HOSTNAME) }
        ''}
  '');

  init.env = pkgs.writeText "init.env" /* sh */ ''

    export HOSTNAME="$(${pkgs.nettools}/bin/hostname)"
    export STOCKHOLM_VERSION="''${STOCKHOLM_VERSION-$(${shell.get-version})}"

    export quiet
    export system
    export target
    export user

    default_target=root@$system:22/var/src

    export target_object="$(
      ${cmds.parse-target} "$target" -d "$default_target"
    )"
    export target_user="$(echo $target_object | ${pkgs.jq}/bin/jq -r .user)"
    export target_host="$(echo $target_object | ${pkgs.jq}/bin/jq -r .host)"
    export target_port="$(echo $target_object | ${pkgs.jq}/bin/jq -r .port)"
    export target_path="$(echo $target_object | ${pkgs.jq}/bin/jq -r .path)"
    export target_local="$(echo $target_object | ${pkgs.jq}/bin/jq -r .local)"
  '';

  init.proxy = command: opts: pkgs.writeText "init.proxy" /* sh */ ''
    if \test "''${using_proxy-}" != true; then

      source=$(${cmds.get-source} "$source_file")
      qualified_target=$target_user@$target_host:$target_port$target_path
      if \test "$force_populate" = true; then
        echo "$source" | ${pkgs.populate}/bin/populate --force "$qualified_target"
      else
        echo "$source" | ${pkgs.populate}/bin/populate "$qualified_target"
      fi

      if \test "$target_local" != true; then
        exec ${pkgs.openssh}/bin/ssh \
            "$target_user@$target_host" -p "$target_port" \
            cd "$target_path/stockholm" \; \
            NIX_PATH=$(${pkgs.quote}/bin/quote "$target_path") \
            STOCKHOLM_VERSION=$(${pkgs.quote}/bin/quote "$STOCKHOLM_VERSION") \
            nix-shell --run "$(${pkgs.quote}/bin/quote "
              ${lib.concatStringsSep " " (lib.mapAttrsToList
                (name: opt: /* sh */
                  "${opt.varname}=\$(${pkgs.quote}/bin/quote ${opt.ref})")
                opts
              )} \
              using_proxy=true \
              ${lib.shell.escape command} \
              $WITHGETOPT_ORIG_ARGS \
            ")"
      fi
    fi
  '';

  utils.build = pkgs.writeDash "utils.build" ''
    set -efu
    ${utils.with-whatsupnix} \
    ${pkgs.nix}/bin/nix-build \
        --no-out-link \
        --show-trace \
        -E "with import <stockholm>; $1" \
        -I "$target_path" \
  '';

  utils.with-whatsupnix = pkgs.writeDash "utils.with-whatsupnix" ''
    set -efu
    if \test "$quiet" = true; then
      "$@" -Q 2>&1 | ${pkgs.whatsupnix}/bin/whatsupnix
    else
      exec "$@"
    fi
  '';

  shell.get-version = pkgs.writeDash "stockholm.get-version" ''
    set -efu
    version=git.$(${pkgs.git}/bin/git describe --always --dirty)
    case $version in (*-dirty)
      version=$version@$HOSTNAME
    esac
    date=$(${pkgs.coreutils}/bin/date +%y.%m)
    echo "$date.$version"
  '';

in

  pkgs.writeOut "stockholm" (lib.mapAttrs' (name: link:
    lib.nameValuePair "/bin/${name}" { inherit link; }
  ) cmds)
