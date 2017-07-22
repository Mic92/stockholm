let
  lib = import ./lib;
  pkgs = import <nixpkgs> { overlays = [(import ./krebs/5pkgs)]; };

  #
  # high level commands
  #

  # usage: deploy [--user=USER] --system=SYSTEM [--target=TARGET]
  cmds.deploy = pkgs.writeDash "cmds.deploy" ''
    set -efu

    command=deploy
    . ${init.args}
    \test -n "''${target-}" || target=$system
    \test -n "''${user-}" || user=$LOGNAME
    . ${init.env}

    exec ${utils.deploy}
  '';

  # usage: test [--user=USER] --system=SYSTEM --target=TARGET
  cmds.test = pkgs.writeDash "cmds.test" /* sh */ ''
    set -efu

    export dummy_secrets=true

    command=test
    . ${init.args}
    \test -n "''${user-}" || user=$LOGNAME
    . ${init.env}

    exec ${utils.build} config.system.build.toplevel
  '';

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

  # usage: parse-target [USER@]HOST[:PORT][/PATH]
  cmds.parse-target = pkgs.writeDash "cmds.parse-target" ''
    set -efu
    script=${pkgs.writeText "cmds.parse-target.jq" ''
      def when(c; f): if c then f else . end;
      def capturesDef(i; v): .captures[i].string | when(. == null; v);
      $target | match("^(?:([^@]+)@)?([^:/]+)?(?::([0-9]+))?(/.*)?$") | {
        user: capturesDef(0; "root"),
        host: capturesDef(1; env.system),
        port: capturesDef(2; "22"),
        path: capturesDef(3; "/var/src"),
      } | . + {
        local: (.user == env.LOGNAME and .host == env.HOSTNAME),
      }
    ''}
    exec ${pkgs.jq}/bin/jq -enrf "$script" --arg target "$1" \
  '';

  init.args = pkgs.writeText "init.args" /* sh */ ''
    args=$(${pkgs.utillinux}/bin/getopt -n "$command" -s sh \
        -o s:t:u: \
        -l system:,target:,user: \
        -- "$@")
    if \test $? != 0; then exit 1; fi
    eval set -- "$args"
    while :; do case $1 in
      -s|--system) system=$2; shift 2;;
      -t|--target) target=$2; shift 2;;
      -u|--user) user=$2; shift 2;;
      --) shift; break;;
    esac; done
    for arg; do echo "$command: bad argument: $arg" >&2; done
    if \test $# != 0; then exit 2; fi
  '';

  init.env = pkgs.writeText "init.env" /* sh */ ''
    source=''${source-$user/1systems/$system/source.nix}

    export source
    export system
    export target
    export user

    export target_object="$(parse-target "$target")"
    export target_user="$(echo $target_object | ${pkgs.jq}/bin/jq -r .user)"
    export target_host="$(echo $target_object | ${pkgs.jq}/bin/jq -r .host)"
    export target_port="$(echo $target_object | ${pkgs.jq}/bin/jq -r .port)"
    export target_path="$(echo $target_object | ${pkgs.jq}/bin/jq -r .path)"
    export target_local="$(echo $target_object | ${pkgs.jq}/bin/jq -r .local)"

    if \test "''${using_proxy-}" != true; then
      ${init.env.populate}
      if \test "$target_local" != true; then
        exec ${init.env.proxy} "$command" "$@"
      fi
    fi
  '' // {
    populate = pkgs.writeDash "init.env.populate" ''
      set -efu
      _source=$(get-source "$source")
      echo $_source |
      ${pkgs.populate}/bin/populate \
          "$target_user@$target_host:$target_port$target_path" \
        >&2
      unset _source
    '';
    proxy = pkgs.writeDash "init.env.proxy" ''
      set -efu
      q() {
        ${pkgs.jq}/bin/jq -nr --arg x "$*" '$x | @sh "\(.)"'
      }
      exec ${pkgs.openssh}/bin/ssh \
        "$target_user@$target_host" -p "$target_port" \
        cd "$target_path/stockholm" \; \
        NIX_PATH=$(q "$target_path") \
        STOCKHOLM_VERSION=$STOCKHOLM_VERSION \
        nix-shell \
            --run $(q \
                system=$system \
                target=$target \
                using_proxy=true \
                "$*"
            )
    '';
  };

  utils.build = pkgs.writeDash "utils.build" ''
    set -efu
    ${pkgs.nix}/bin/nix-build \
        -Q \
        --no-out-link \
        --show-trace \
        -E "with import <stockholm>; $1" \
        -I "$target_path" \
      2>&1 |
    ${pkgs.whatsupnix}/bin/whatsupnix
  '';

  utils.deploy = pkgs.writeDash "utils.deploy" ''
    set -efu
    PATH=/run/current-system/sw/bin nixos-rebuild switch \
        -Q \
        --show-trace \
        -I "$target_path" \
      2>&1 |
    ${pkgs.whatsupnix}/bin/whatsupnix
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
    export NIX_PATH=stockholm=$PWD:nixpkgs=${toString <nixpkgs>}
    export NIX_REMOTE=daemon
    export PATH=${lib.makeBinPath [
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
