{ compiler ? "default" }: let

  stockholm = import <stockholm>;

  inherit (stockholm.systems.${lib.krops.getHostName}) config pkgs;
  inherit (stockholm) lib;

  haskellPackages =
    if compiler == "default"
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};

  xmonadDrv = haskellPackages.callPackage (import ./.) {};

in

  lib.overrideDerivation xmonadDrv.env (oldAttrs: {
    shellHook = ''
      pkg_name=${lib.shell.escape (lib.baseNameOf (toString ./.))}

      WORKDIR=${toString ./src}
      CACHEDIR=$HOME/tmp/$pkg_name
      HISTFILE=$CACHEDIR/bash_history

      mkdir -p "$CACHEDIR"

      config_XMONAD_CACHE_DIR=${lib.shell.escape
        config.systemd.services.xmonad.environment.XMONAD_CACHE_DIR
      }

      xmonad=$CACHEDIR/xmonad-${lib.currentSystem}

      xmonad_build() {(
        set -efu
        cd "$WORKDIR"
        options=$(
          ${pkgs.cabal-read}/bin/ghc-options "$WORKDIR/$pkg_name.cabal" xmonad
        )
        ghc $options \
            -odir "$CACHEDIR" \
            -hidir "$CACHEDIR" \
            -o "$xmonad" \
            main.hs
      )}

      xmonad_restart() {(
        set -efu
        cd "$WORKDIR"
        if systemctl is-active xmonad; then
          sudo systemctl stop xmonad
          cp -b "$config_XMONAD_CACHE_DIR"/xmonad.state "$CACHEDIR"/
          echo "xmonad.state: $(cat "$CACHEDIR"/xmonad.state)"
        else
          "$xmonad" --shutdown || :
        fi
        "$xmonad" &
        echo xmonad pid: $! >&2
      )}

      xmonad_yield() {(
        set -efu
        "$xmonad" --shutdown
        cp -b "$CACHEDIR"/xmonad.state "$config_XMONAD_CACHE_DIR"/
        sudo systemctl start xmonad
      )}

      export PATH=${config.systemd.services.xmonad.path}:$PATH
      export SHELL=/run/current-system/sw/bin/bash

      export XMONAD_CACHE_DIR="$CACHEDIR"
      export XMONAD_DATA_DIR="$CACHEDIR"
      export XMONAD_CONFIG_DIR=/var/empty

      unset XMONAD_STARTUP_HOOK

      cd "$WORKDIR"
    '';
  })
