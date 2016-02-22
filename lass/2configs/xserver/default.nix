{ config, lib, pkgs, ... }@args:

with config.krebs.lib;

let
  # TODO krebs.build.user
  user = config.users.users.mainUser;

  out = {
    services.xserver = {
      display = 11;
      tty = 11;

      synaptics = {
        enable = true;
        twoFingerScroll = true;
        accelFactor = "0.035";
      };

      #keyboard stuff
      layout = "us";
      xkbVariant = "altgr-intl";
      xkbOptions = "caps:backspace";
    };

    fonts.fonts = [
      pkgs.xlibs.fontschumachermisc
    ];

    systemd.services.urxvtd = {
      wantedBy = [ "multi-user.target" ];
      reloadIfChanged = true;
      serviceConfig = {
        ExecReload = need-reload "urxvtd.service";
        ExecStart = "${pkgs.rxvt_unicode}/bin/urxvtd";
        Restart = "always";
        RestartSec = "2s";
        StartLimitBurst = 0;
        User = user.name;
      };
    };

    security.setuidPrograms = [
      "slock"
    ];

    systemd.services.display-manager.enable = false;

    services.xserver.enable = true;

    systemd.services.xmonad = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "xserver.service" ];
      environment = xmonad-environment;
      serviceConfig = {
        ExecStart = "${xmonad-start}/bin/xmonad";
        ExecStop = "${xmonad-stop}/bin/xmonad-stop";
        User = user.name;
        WorkingDirectory = user.home;
      };
    };

    systemd.services.xserver = {
      after = [
        "systemd-udev-settle.service"
        "local-fs.target"
        "acpid.service"
      ];
      reloadIfChanged = true;
      environment = xserver-environment;
      serviceConfig = {
        ExecReload = need-reload "xserver.service";
        ExecStart = "${xserver}/bin/xserver";
      };
    };
  };

  xmonad-environment = {
    DISPLAY = ":${toString config.services.xserver.display}";
    XMONAD_STATE = "/tmp/xmonad.state";

    # XXX JSON is close enough :)
    XMONAD_WORKSPACES0_FILE = pkgs.writeText "xmonad.workspaces0" (toJSON [
      "cr"
      "gm"
      "ff"
      "IM"
      "mail"
      "stockholm"
    ]);
  };

  xmonad-start = pkgs.writeScriptBin "xmonad" ''
    #! ${pkgs.bash}/bin/bash
    set -efu
    export PATH; PATH=${makeSearchPath "bin" ([
      pkgs.rxvt_unicode
    ] ++ config.environment.systemPackages)}:/var/setuid-wrappers
    settle() {(
      # Use PATH for a clean journal
      command=''${1##*/}
      PATH=''${1%/*}; export PATH
      shift
      until "$command" "$@"; do
        ${pkgs.coreutils}/bin/sleep 1
      done
    )&}
    settle ${pkgs.xorg.xhost}/bin/xhost +LOCAL:
    settle ${pkgs.xorg.xrdb}/bin/xrdb -merge ${import ./Xresources.nix args}
    settle ${pkgs.xorg.xsetroot}/bin/xsetroot -solid '#1c1c1c'
    exec ${pkgs.xmonad-lass}/bin/xmonad
  '';

  xmonad-stop = pkgs.writeScriptBin "xmonad-stop" ''
    #! /bin/sh
    exec ${pkgs.xmonad-lass}/bin/xmonad --shutdown
  '';

  xserver-environment = {
    XKB_BINDIR = "${pkgs.xorg.xkbcomp}/bin"; # Needed for the Xkb extension.
    XORG_DRI_DRIVER_PATH = "/run/opengl-driver/lib/dri"; # !!! Depends on the driver selected at runtime.
    LD_LIBRARY_PATH = concatStringsSep ":" (
      [ "${pkgs.xorg.libX11}/lib" "${pkgs.xorg.libXext}/lib" ]
      ++ concatLists (catAttrs "libPath" config.services.xserver.drivers));
  };

  xserver = pkgs.writeScriptBin "xserver" ''
    #! /bin/sh
    set -efu
    exec ${pkgs.xorg.xorgserver}/bin/X \
        :${toString config.services.xserver.display} \
        vt${toString config.services.xserver.tty} \
        -config ${import ./xserver.conf.nix args} \
        -logfile /var/log/X.${toString config.services.xserver.display}.log \
        -nolisten tcp \
        -xkbdir ${pkgs.xkeyboard_config}/etc/X11/xkb \
  '';

  need-reload = s: let
    pkg = pkgs.writeScriptBin "need-reload" ''
      #! /bin/sh
      echo "$*"
    '';
  in "${pkg}/bin/need-reload ${s}";

in out
