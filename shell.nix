let
  lib = import ./lib;
  pkgs = import <nixpkgs> { overlays = [(import ./krebs/5pkgs)]; };

  #
  # high level commands
  #

  cmds.deploy = pkgs.withGetopt {
    force-populate = { default = /* sh */ "false"; switch = true; };
    quiet = { default = /* sh */ "false"; switch = true; };
    source_file = {
      default = /* sh */ "$user/1systems/$system/source.nix";
      long = "source";
    };
    system = {};
    target.default = /* sh */ "$system";
    user.default = /* sh */ "$LOGNAME";
  } (opts: pkgs.writeDash "cmds.deploy" ''
    set -efu

    . ${init.env}
    . ${init.proxy opts}

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
  } (opts: pkgs.writeBash "cmds.install" ''
    set -efu

    . ${init.env}

    if \test "''${using_proxy-}" != true; then
      ${pkgs.openssh}/bin/ssh \
          -o StrictHostKeyChecking=no \
          -o UserKnownHostsFile=/dev/null \
          "$target_user@$target_host" -p "$target_port" \
          env target_path=$(quote "$target_path") \
              sh -s prepare < ${./krebs/4lib/infest/prepare.sh}
              # TODO inline prepare.sh?
    fi

    . ${init.proxy opts}

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
  } (opts: pkgs.writeDash "cmds.test" /* sh */ ''
    set -efu

    export dummy_secrets=true

    . ${init.env}
    . ${init.proxy opts}

    exec ${utils.build} config.system.build.toplevel
  '');

  #
  # low level commands
  #

  # usage: get-source SOURCE_FILE
  cmds.get-source = pkgs.writeDash "cmds.get-source" ''
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
  } (opts: pkgs.writeDash "cmds.parse-target" ''
    set -efu
    target=$1; shift
    for arg; do echo "$0: bad argument: $arg" >&2; done
    if \test $# != 0; then exit 2; fi
    exec ${pkgs.jq}/bin/jq \
        -enr \
        --arg default_target "$default_target" \
        --arg target "$target" \
        -f ${pkgs.writeText "cmds.parse-target.jq" ''
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

  # usage: quote [ARGS...]
  cmds.quote = pkgs.writeDash "cmds.quote" ''
    set -efu
    prefix=
    for x; do
      y=$(${pkgs.jq}/bin/jq -nr --arg x "$x" '$x | @sh "\(.)"')
      echo -n "$prefix$y"
      prefix=' '
    done
    echo
  '';

  init.env = pkgs.writeText "init.env" /* sh */ ''
    export quiet
    export system
    export target
    export user

    default_target=root@$system:22/var/src

    export target_object="$(parse-target "$target" -d "$default_target")"
    export target_user="$(echo $target_object | ${pkgs.jq}/bin/jq -r .user)"
    export target_host="$(echo $target_object | ${pkgs.jq}/bin/jq -r .host)"
    export target_port="$(echo $target_object | ${pkgs.jq}/bin/jq -r .port)"
    export target_path="$(echo $target_object | ${pkgs.jq}/bin/jq -r .path)"
    export target_local="$(echo $target_object | ${pkgs.jq}/bin/jq -r .local)"
  '';

  init.proxy = opts: pkgs.writeText "init.proxy" /* sh */ ''
    if \test "''${using_proxy-}" != true; then

      source=$(get-source "$source_file")
      qualified_target=$target_user@$target_host:$target_port$target_path
      if \test "$force_populate" = true; then
        echo "$source" | populate --force "$qualified_target"
      else
        echo "$source" | populate "$qualified_target"
      fi

      if \test "$target_local" != true; then
        exec ${pkgs.openssh}/bin/ssh \
            "$target_user@$target_host" -p "$target_port" \
            cd "$target_path/stockholm" \; \
            NIX_PATH=$(quote "$target_path") \
            STOCKHOLM_VERSION=$(quote "$STOCKHOLM_VERSION") \
            nix-shell --run "$(quote "
              ${lib.concatStringsSep " " (lib.mapAttrsToList
                (name: opt: /* sh */ "${opt.varname}=\$(quote ${opt.ref})")
                opts
              )} \
              using_proxy=true \
              $(quote "$0" "$@")
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

  shell.get-version = pkgs.writeDash "shell.get-version" ''
    set -efu
    version=git.$(${pkgs.git}/bin/git describe --always --dirty)
    case $version in (*-dirty)
      version=$version@$HOSTNAME
    esac
    date=$(${pkgs.coreutils}/bin/date +%y.%m)
    echo "$date.$version"
  '';

  shell.cmdspkg = pkgs.writeOut "shell.cmdspkg" (lib.mapAttrs' (name: link:
    lib.nameValuePair "/bin/${name}" { inherit link; }
  ) cmds);

in pkgs.stdenv.mkDerivation {
  name = "stockholm";
  shellHook = /* sh */ ''
    export OLD_PATH="$PATH"
    export NIX_PATH=stockholm=${toString ./.}:nixpkgs=${toString <nixpkgs>}
    if test -e /nix/var/nix/daemon-socket/socket; then
      export NIX_REMOTE=daemon
    fi
    export PATH=${lib.makeBinPath [
      pkgs.populate
      shell.cmdspkg
    ]}

    eval "$(declare -F | ${pkgs.gnused}/bin/sed s/declare/unset/)"
    shopt -u no_empty_cmd_completion
    unalias -a

    enable -n \
        . [ alias bg bind break builtin caller cd command compgen complete \
        compopt continue dirs disown eval exec false fc fg getopts hash \
        help history jobs kill let local logout mapfile popd printf pushd \
        pwd read readarray readonly shift source suspend test times trap \
        true typeset ulimit umask unalias wait

    exitHandler() {
      :
    }

    export HOSTNAME="$(${pkgs.nettools}/bin/hostname)"
    export STOCKHOLM_VERSION="''${STOCKHOLM_VERSION-$(${shell.get-version})}"

    PS1='\[\e[38;5;162m\]\w\[\e[0m\] '
  '';
}
