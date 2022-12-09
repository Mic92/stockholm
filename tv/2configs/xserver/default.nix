with import ./lib;
{ config, pkgs, ... }@args: let
  cfg = {
    cacheDir = cfg.dataDir;
    configDir = "/var/empty";
    dataDir = "/run/xdg/${cfg.user.name}/xmonad";
    user = config.krebs.build.user;
    xmonad.pkg = pkgs.haskellPackages.xmonad-tv.overrideAttrs (_: {
      au = {
        XMONAD_BUILD_SCREEN_WIDTH = 1920;
        XMONAD_BUILD_TERM_FONT_WIDTH = 10;
        XMONAD_BUILD_TERM_FONT = "xft:Input Mono:size=12:style=Regular";
        XMONAD_BUILD_TERM_PADDING = 2;
      };
    }.${config.krebs.build.host.name} or {
      XMONAD_BUILD_SCREEN_WIDTH = 1366;
      XMONAD_BUILD_TERM_FONT_WIDTH = 6;
      XMONAD_BUILD_TERM_FONT = "-*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1";
      XMONAD_BUILD_TERM_PADDING = 2;
    });
  };
in {

  imports = [
    ./sxiv.nix
    ./urxvt.nix
  ];

  environment.systemPackages = [
    pkgs.ff
    pkgs.font-size
    pkgs.gitAndTools.qgit
    pkgs.mpv
    pkgs.xdotool
    pkgs.xsel
    pkgs.zathura
  ];

  fonts.fonts = [
    pkgs.xorg.fontschumachermisc
  ];

  services.xserver = {

    # Don't install feh into systemPackages
    # refs <nixpkgs/nixos/modules/services/x11/desktop-managers>
    desktopManager.session = mkForce [];

    displayManager.lightdm.enable = mkForce false;
    displayManager.job.execCmd = mkForce "derp";

    enable = true;
    display = mkForce 11;
    tty = mkForce 11;

    synaptics = {
      enable = true;
      twoFingerScroll = true;
      accelFactor = "0.035";
    };
  };

  systemd.services.display-manager.enable = false;

  systemd.services.xmonad = let
    xmonad = "${cfg.xmonad.pkg}/bin/xmonad";
    xmonad-start = pkgs.writeDash "xmonad-start" ''
      ${pkgs.coreutils}/bin/mkdir -p "$XMONAD_CACHE_DIR"
      ${pkgs.coreutils}/bin/mkdir -p "$XMONAD_CONFIG_DIR"
      ${pkgs.coreutils}/bin/mkdir -p "$XMONAD_DATA_DIR"

      f=$HOME/.dbus/session-bus/$(${pkgs.coreutils}/bin/cat /etc/machine-id)-${
        toString config.services.xserver.display
      }
      if test -e "$f" &&
          . "$f" &&
          ${pkgs.coreutils}/bin/kill -0 "$DBUS_SESSION_BUS_PID"
      then
        export DBUS_SESSION_BUS_ADDRESS
      else
        eval "$(${pkgs.dbus.lib}/bin/dbus-launch --sh-syntax)"
      fi

      exec ${xmonad}
    '';
    xmonad-ready = pkgs.writeDash "xmonad-ready" ''
      ${pkgs.systemd}/bin/systemd-notify --ready
      {
        ${pkgs.xorg.xhost}/bin/xhost +SI:localuser:${cfg.user.name}
        ${pkgs.xorg.xhost}/bin/xhost -LOCAL:
      } &
      ${pkgs.xorg.xmodmap}/bin/xmodmap ${import ./Xmodmap.nix args} &
      ${pkgs.xorg.xsetroot}/bin/xsetroot -solid '#1c1c1c' &
      wait
    '';
  in {
    wantedBy = [ "graphical.target" ];
    requires = [ "xserver.service" ];
    environment = {
      DISPLAY = ":${toString config.services.xserver.display}";
      FZMENU_FZF_DEFAULT_OPTS = toString [
        "--color=dark,border:126,bg+:090"
        "--inline-info"
      ];
      XMONAD_CACHE_DIR = cfg.cacheDir;
      XMONAD_CONFIG_DIR = cfg.configDir;
      XMONAD_DATA_DIR = cfg.dataDir;
      XMONAD_STARTUP_HOOK = xmonad-ready;
      XMONAD_WORKSPACES0_FILE = pkgs.writeJSON "xmonad-workspaces0.json" [
        "Dashboard" # we start here
        "23"
        "cr"
        "ff"
        "hack"
        "im"
        "mail"
        "stockholm"
        "za" "zh" "zj" "zs"
      ];
    };
    path = [
      config.tv.slock.package
      (pkgs.flameshot-once.override {
        config.imgur.enable = true;
        config.imgur.createUrl = "http://ni.r/image";
        config.imgur.deleteUrl = "http://ni.r/image/delete/%1";
        config.imgur.xdg-open.browser = "/etc/profiles/per-user/tv/bin/cr";
        config.timeout = 200;
      })
      pkgs.pulseaudio.out
      pkgs.rxvt_unicode
      pkgs.xcalib
      "/run/wrappers" # for su
    ];
    serviceConfig = {
      ExecStart = "@${xmonad-start} xmonad-${currentSystem}";
      ExecStop = "@${xmonad} xmonad-${currentSystem} --shutdown";
      SyslogIdentifier = "xmonad";
      User = cfg.user.name;
      WorkingDirectory = cfg.user.home;

      NotifyAccess = "all";
      Type = "notify";
    };
  };

  systemd.services.xserver = {
    after = [
      "acpid.service"
      "local-fs.target"
      "systemd-udev-settle.service"
    ];
    wants = [
      "systemd-udev-settle.service"
    ];
    restartIfChanged = false;
    environment = {
      LD_LIBRARY_PATH = concatStringsSep ":" ([ "/run/opengl-driver/lib" ]
        ++ concatLists (catAttrs "libPath" config.services.xserver.drivers));
    };
    serviceConfig = {
      SyslogIdentifier = "xserver";
      ExecStart = toString [
        "${pkgs.xorg.xorgserver}/bin/X"
        ":${toString config.services.xserver.display}"
        "vt${toString config.services.xserver.tty}"
        "-config ${import ./xserver.conf.nix args}"
        "-logfile /dev/null -logverbose 0 -verbose 3"
        "-nolisten tcp"
        "-xkbdir ${config.services.xserver.xkbDir}"
      ];
    };
  };

  tv.slock = {
    enable = true;
    user = cfg.user;
  };
}
