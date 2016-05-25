{ config, lib, pkgs, ... }@args:

with config.krebs.lib;

let
  # TODO krebs.build.user
  user = config.users.users.tv;

  out = {
    services.xserver.display = 11;
    services.xserver.tty = 11;

    services.xserver.synaptics = {
      enable = true;
      twoFingerScroll = true;
      accelFactor = "0.035";
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

    environment.systemPackages = [
      pkgs.ff
      pkgs.gitAndTools.qgit
      pkgs.mpv
      pkgs.slock
      pkgs.sxiv
      pkgs.xsel
      pkgs.zathura
    ];

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
        ExecStart = "${pkgs.xmonad-tv}/bin/xmonad-tv";
        ExecStop = "${pkgs.xmonad-tv}/bin/xmonad-tv --shutdown";
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
        ExecStart = toString [
          "${pkgs.xorg.xorgserver}/bin/X"
          ":${toString config.services.xserver.display}"
          "vt${toString config.services.xserver.tty}"
          "-config ${import ./xserver.conf.nix args}"
          "-logfile /var/log/X.${toString config.services.xserver.display}.log"
          "-nolisten tcp"
          "-xkbdir ${pkgs.xkeyboard_config}/etc/X11/xkb"
        ];
      };
    };
  };

  xmonad-environment = {
    DISPLAY = ":${toString config.services.xserver.display}";

    XMONAD_STARTUP_HOOK = pkgs.writeDash "xmonad-startup-hook" ''
      ${pkgs.xorg.xhost}/bin/xhost +LOCAL: &
      ${pkgs.xorg.xmodmap}/bin/xmodmap ${import ./Xmodmap.nix args} &
      ${pkgs.xorg.xrdb}/bin/xrdb -merge ${import ./Xresources.nix args} &
      ${pkgs.xorg.xsetroot}/bin/xsetroot -solid '#1c1c1c' &
      wait
    '';

    XMONAD_STATE = "/tmp/xmonad.state";

    # XXX JSON is close enough :)
    XMONAD_WORKSPACES0_FILE = pkgs.writeText "xmonad.workspaces0" (toJSON [
      "Dashboard" # we start here
      "23"
      "cr"
      "ff"
      "hack"
      "im"
      "mail"
      "stockholm"
      "za" "zh" "zj" "zs"
    ]);
  };

  xserver-environment = {
    XKB_BINDIR = "${pkgs.xorg.xkbcomp}/bin"; # Needed for the Xkb extension.
    XORG_DRI_DRIVER_PATH = "/run/opengl-driver/lib/dri"; # !!! Depends on the driver selected at runtime.
    LD_LIBRARY_PATH = concatStringsSep ":" (
      [ "${pkgs.xorg.libX11}/lib" "${pkgs.xorg.libXext}/lib" ]
      ++ concatLists (catAttrs "libPath" config.services.xserver.drivers));
  };

  need-reload = s: let
    pkg = pkgs.writeScriptBin "need-reload" ''
      #! /bin/sh
      echo "$*"
    '';
  in "${pkg}/bin/need-reload ${s}";

in out
