let
  lib = import ./lib;
  pkgs = import <nixpkgs> { overlays = [(import ./krebs/5pkgs)]; };

  get-version = pkgs.writeDash "get-version" ''
    set -efu
    version=git.$(${pkgs.git}/bin/git describe --always --dirty)
    case $version in (*-dirty)
      version=$version@$HOSTNAME
    esac
    date=$(${pkgs.coreutils}/bin/date +%y.%m)
    echo "$date.$version"
  '';

in pkgs.stdenv.mkDerivation {
  name = "stockholm";
  shellHook = /* sh */ ''
    export OLD_PATH="$PATH"
    export NIX_PATH=stockholm=${toString ./.}:nixpkgs=${toString <nixpkgs>}
    if test -e /nix/var/nix/daemon-socket/socket; then
      export NIX_REMOTE=daemon
    fi
    export PATH=${lib.makeBinPath [
      pkgs.stockholm
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
    export STOCKHOLM_VERSION="''${STOCKHOLM_VERSION-$(${get-version})}"

    PS1='\[\e[38;5;162m\]\w\[\e[0m\] '
  '';
}
