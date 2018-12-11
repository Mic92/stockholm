{ config, pkgs, ... }@args:
with import <stockholm/lib>;
let
  cfg = {
    cacheDir = cfg.dataDir;
    configDir = "/var/empty";
    dataDir = "/run/xdg/${cfg.user.name}/xmonad";
    user = config.krebs.build.user;
  };
in {

  environment.systemPackages = [
    pkgs.ff
    pkgs.font-size
    pkgs.gitAndTools.qgit
    pkgs.mpv
    pkgs.sxiv
    pkgs.xdotool
    pkgs.xsel
    pkgs.zathura
  ];

  fonts.fonts = [
    pkgs.xlibs.fontschumachermisc
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
    xmonad = "${pkgs.haskellPackages.xmonad-tv}/bin/xmonad";
    xmonad-prepare = pkgs.writeDash "xmonad-prepare" ''
      ${pkgs.coreutils}/bin/mkdir -p "$XMONAD_CACHE_DIR"
      ${pkgs.coreutils}/bin/mkdir -p "$XMONAD_CONFIG_DIR"
      ${pkgs.coreutils}/bin/mkdir -p "$XMONAD_DATA_DIR"
    '';
    xmonad-ready = pkgs.writeDash "xmonad-ready" ''
      {
        ${pkgs.xorg.xhost}/bin/xhost +SI:localuser:${cfg.user.name}
        ${pkgs.xorg.xhost}/bin/xhost -LOCAL:
      } &
      ${pkgs.xorg.xmodmap}/bin/xmodmap ${import ./Xmodmap.nix args} &
      ${pkgs.xorg.xrdb}/bin/xrdb ${import ./Xresources.nix args} &
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
      pkgs.fzmenu
      pkgs.pulseaudioLight.out
      pkgs.rxvt_unicode
      pkgs.xcalib
      "/run/wrappers" # for su
    ];
    serviceConfig = {
      ExecStartPre = "@${xmonad-prepare} xmonad-prepare";
      ExecStart = "@${xmonad} xmonad-${currentSystem}";
      ExecStop = "@${xmonad} xmonad-${currentSystem} --shutdown";
      SyslogIdentifier = "xmonad";
      User = cfg.user.name;
      WorkingDirectory = cfg.user.home;
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

  systemd.services.urxvtd = {
    wantedBy = [ "graphical.target" ];
    restartIfChanged = false;
    serviceConfig = {
      SyslogIdentifier = "urxvtd";
      ExecStart = "${pkgs.rxvt_unicode}/bin/urxvtd";
      Restart = "always";
      RestartSec = "2s";
      StartLimitBurst = 0;
      User = cfg.user.name;
    };
  };

  tv.slock = {
    enable = true;
    user = cfg.user;
  };
}
