{ nixpkgs ? import <nixpkgs> {} }: let

  inherit (nixpkgs) lib pkgs;
  slib = import ./lib;
  spkgs = {
    populate = pkgs.callPackage ./krebs/5pkgs/simple/populate {};
    whatsupnix = pkgs.callPackage ./krebs/5pkgs/simple/whatsupnix {};
  };

  # usage: deploy system=SYSTEM [target=TARGET]
  cmds.deploy = pkgs.writeScript "cmds.deploy" /* sh */ ''
    #! ${pkgs.dash}/bin/dash
    set -efu

    command=deploy
    . ${init.args}
    \test -n "''${target-}" || target=$system
    . ${init.env}

    exec ${utils.deploy}
  '';

  # usage: test system=SYSTEM target=TARGET
  cmds.test = pkgs.writeScript "cmds.test" /* sh */ ''
    #! ${pkgs.dash}/bin/dash
    set -efu

    command=test
    . ${init.args}
    . ${init.env}

    export dummy_secrets=true
    exec ${utils.build} config.system.build.toplevel
  '';

  init.args = pkgs.writeText "init.args" /* sh */ ''
    fail=
    for arg; do
      case $arg in
        system=*) system=''${arg#*=};;
        target=*) target=''${arg#*=};;
        *) echo "$command: bad argument: $arg" >&2; fail=1
      esac
    done
    if \test -n "$fail"; then  
      exit 1
    fi
    unset fail
  '';

  init.env = pkgs.writeText "init.env" /* sh */ ''
    config=''${config-$LOGNAME/1systems/$system.nix}

    export config
    export system
    export target

    export target_object="$(${init.env.parsetarget} $target)"
    export target_user="$(echo $target_object | ${pkgs.jq}/bin/jq -r .user)"
    export target_host="$(echo $target_object | ${pkgs.jq}/bin/jq -r .host)"
    export target_port="$(echo $target_object | ${pkgs.jq}/bin/jq -r .port)"
    export target_path="$(echo $target_object | ${pkgs.jq}/bin/jq -r .path)"
    export target_local="$(echo $target_object | ${pkgs.jq}/bin/jq -r .local)"

    export qtarget="$target_user@$target_host:$target_port$target_path"

    ${init.env.populate}

    if \test "$target_local" != true && \test "''${DISABLE_PROXY-}" != 1; then
      exec ${init.env.proxy} "$command" "$@"
    fi
  '' // {
    parsetarget = pkgs.writeScript "init.env.parsetarget" /* sh */ ''
      #! ${pkgs.dash}/bin/dash
      set -efu
      exec ${pkgs.jq}/bin/jq \
          -enr \
          --arg target "$1" \
          -f ${init.env.parsetarget.jq}
    '' // {
      jq = pkgs.writeText "init.env.parsetarget.jq" ''
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
      '';
    };
    populate = pkgs.writeScript "init.env.populate" /* sh */ ''
      #! ${pkgs.dash}/bin/dash
      set -efu
      if \test "''${DISABLE_POPULATE-}" = 1; then
        exit
      fi
      set -x
      ${pkgs.nix}/bin/nix-instantiate \
          --eval \
          --json \
          --readonly-mode \
          --show-trace \
          --strict \
          -I nixos-config="$config" \
          -E 'with import <stockholm>; config.krebs.build.source' \
        |
      ${spkgs.populate}/bin/populate "$qtarget" >&2
    '';
    proxy = pkgs.writeScript "init.env.proxy" /* sh */ ''
      #! ${pkgs.dash}/bin/dash
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
            --command $(q \
                config=$config \
                system=$system \
                target=$target \
                DISABLE_POPULATE=1 \
                DISABLE_PROXY=1 \
                "$*"
            )
    '';
  };

  utils.build = pkgs.writeScript "utils.build" /* sh */ ''
    #! ${pkgs.dash}/bin/dash
    set -efu
    expr=$1
    shift
    ${pkgs.nix}/bin/nix-build \
        -Q \
        --no-out-link \
        --show-trace \
        -E "with import <stockholm>; $expr" \
        -I "$target_path" \
        "$@" \
      2>&1 |
    ${pkgs.coreutils}/bin/stdbuf -oL ${spkgs.whatsupnix}/bin/whatsupnix
  '';

  utils.deploy = pkgs.writeScript "utils.deploy" /* sh */ ''
    #! ${pkgs.dash}/bin/dash
    set -efu
    PATH=/run/current-system/sw/bin nixos-rebuild \
        switch \
        -Q \
        --show-trace \
        -I "$target_path" \
        "$@" \
      2>&1 |
    ${pkgs.coreutils}/bin/stdbuf -oL ${spkgs.whatsupnix}/bin/whatsupnix
  '';

  hook.get-version = pkgs.writeScript "hook.get-version" /* sh */ ''
    #! ${pkgs.dash}/bin/dash
    set -efu
    version=git.$(${pkgs.git}/bin/git describe --always --dirty)
    case $version in (*-dirty)
      version=$version@$HOSTNAME
    esac
    date=$(${pkgs.coreutils}/bin/date +%y.%m)
    echo "$date.$version"
  '';

  hook.pkg = pkgs.runCommand "hook.pkg" {} /* sh */ ''
    mkdir -p $out/bin
    ${lib.concatStrings (lib.mapAttrsToList (name: path: /* sh */ ''
      ln -s ${path} $out/bin/${name}
    '') cmds)}
  '';

in pkgs.stdenv.mkDerivation {
  name = "stockholm";
  shellHook = ''
    export NIX_PATH="stockholm=$PWD''${NIX_PATH+:$NIX_PATH}"
    export PATH=${lib.makeBinPath [
      hook.pkg
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
    export STOCKHOLM_VERSION="''${STOCKHOLM_VERSION-$(${hook.get-version})}"

    PS1='\[\e[38;5;162m\]\w\[\e[0m\] '
  '';
}
