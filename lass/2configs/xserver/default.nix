{ config, pkgs, ... }@args:
with import <stockholm/lib>;
let
  user = config.krebs.build.user;

  copyqConfig = pkgs.writeDash "copyq-config" ''
    ${pkgs.copyq}/bin/copyq config check_clipboard true
    ${pkgs.copyq}/bin/copyq config check_selection true
    ${pkgs.copyq}/bin/copyq config copy_clipboard true
    ${pkgs.copyq}/bin/copyq config copy_selection true

    ${pkgs.copyq}/bin/copyq config activate_closes true
    ${pkgs.copyq}/bin/copyq config clipboard_notification_lines 0
    ${pkgs.copyq}/bin/copyq config clipboard_tab &clipboard
    ${pkgs.copyq}/bin/copyq config disable_tray true
    ${pkgs.copyq}/bin/copyq config hide_tabs true
    ${pkgs.copyq}/bin/copyq config hide_toolbar true
    ${pkgs.copyq}/bin/copyq config item_popup_interval true
    ${pkgs.copyq}/bin/copyq config maxitems 1000
    ${pkgs.copyq}/bin/copyq config move true
    ${pkgs.copyq}/bin/copyq config text_wrap true
  '';
in {

  environment.systemPackages = [
    pkgs.gitAndTools.qgit
    pkgs.mpv
    pkgs.sxiv
    pkgs.xsel
    pkgs.zathura
  ];

  fonts.fonts = [
    pkgs.xlibs.fontschumachermisc
  ];

  services.xserver = {
    enable = true;
    display = 11;
    tty = 11;

    synaptics = {
      enable = true;
      twoFingerScroll = true;
      accelFactor = "0.035";
    };

    layout = "us";
    xkbVariant = "altgr-intl";
    xkbOptions = "caps:backspace";
  };

  systemd.services.display-manager.enable = false;

  systemd.services.xmonad = {
    wantedBy = [ "multi-user.target" ];
    requires = [ "xserver.service" ];
    environment = {
      DISPLAY = ":${toString config.services.xserver.display}";

      XMONAD_STARTUP_HOOK = pkgs.writeDash "xmonad-startup-hook" ''
        ${pkgs.xorg.xhost}/bin/xhost +LOCAL: &
        ${pkgs.xorg.xrdb}/bin/xrdb -merge ${import ./Xresources.nix args} &
        ${pkgs.xorg.xsetroot}/bin/xsetroot -solid '#1c1c1c' &
        wait
      '';

      XMONAD_STATE = "/tmp/xmonad.state";

      # XXX JSON is close enough :)
      XMONAD_WORKSPACES0_FILE = pkgs.writeText "xmonad.workspaces0" (toJSON [
        "dashboard" # we start here
      ]);
    };
    serviceConfig = {
      SyslogIdentifier = "xmonad";
      ExecStart = "${pkgs.xmonad-lass}/bin/xmonad";
      ExecStop = pkgs.writeScript "xmonad-stop" ''
        #! /bin/sh
        ${pkgs.xmonad-lass}/bin/xmonad --shutdown
        ${pkgs.coreutils}/bin/sleep 2s
      '';
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
    environment = {
      XKB_BINDIR = "${pkgs.xorg.xkbcomp}/bin"; # Needed for the Xkb extension.
      XORG_DRI_DRIVER_PATH = "/run/opengl-driver/lib/dri"; # !!! Depends on the driver selected at runtime.
      LD_LIBRARY_PATH = concatStringsSep ":" (
        [ "${pkgs.xorg.libX11}/lib" "${pkgs.xorg.libXext}/lib" ]
        ++ concatLists (catAttrs "libPath" config.services.xserver.drivers));
    };
    serviceConfig = {
      SyslogIdentifier = "xserver";
      ExecReload = "${pkgs.coreutils}/bin/echo NOP";
      ExecStart = toString [
        "${pkgs.xorg.xorgserver}/bin/X"
        ":${toString config.services.xserver.display}"
        "vt${toString config.services.xserver.tty}"
        "-config ${import ./xserver.conf.nix args}"
        "-logfile /dev/null -logverbose 0 -verbose 3"
        "-nolisten tcp"
        "-xkbdir ${pkgs.xkeyboard_config}/etc/X11/xkb"
      ];
    };
  };

  systemd.services.urxvtd = {
    wantedBy = [ "multi-user.target" ];
    reloadIfChanged = true;
    serviceConfig = {
      SyslogIdentifier = "urxvtd";
      ExecReload = "${pkgs.coreutils}/bin/echo NOP";
      ExecStart = "${pkgs.rxvt_unicode}/bin/urxvtd";
      Restart = "always";
      RestartSec = "2s";
      StartLimitBurst = 0;
      User = user.name;
    };
  };

  systemd.services.copyq = {
    wantedBy = [ "multi-user.target" ];
    requires = [ "xserver.service" ];
    environment = {
      DISPLAY = ":${toString config.services.xserver.display}";
    };
    serviceConfig = {
      SyslogIdentifier = "copyq";
      ExecStart = "${pkgs.copyq}/bin/copyq";
      ExecStartPost = copyqConfig;
      Restart = "always";
      RestartSec = "2s";
      StartLimitBurst = 0;
      User = user.name;
    };
  };
}
