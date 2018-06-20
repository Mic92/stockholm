{ config, pkgs, ... }:
with import <stockholm/lib>;
let
  user = config.krebs.build.user;
  xmonad-lass = pkgs.callPackage <stockholm/lass/5pkgs/custom/xmonad-lass> { inherit config; };
in {
  imports = [
    ./mpv.nix
    ./power-action.nix
    ./copyq.nix
    ./livestream.nix
    ./urxvt.nix
    ./network-manager.nix
    {
      hardware.pulseaudio = {
        enable = true;
        systemWide = true;
      };
      security.rtkit.enable = true;
      sound.enableOSSEmulation = false;
    }
    {
      krebs.per-user.lass.packages = [
        pkgs.sshuttle
      ];
      security.sudo.extraConfig = ''
        lass ALL= (root) NOPASSWD:SETENV: ${pkgs.sshuttle}/bin/.sshuttle-wrapped
      '';
    }
    { #font magic
      options.lass.fonts = {
        regular = mkOption {
          type = types.str;
          default = "-*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1";
        };
        bold = mkOption {
          type = types.str;
          default = "-*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1";
        };
        italic = mkOption {
          type = types.str;
          default = "-*-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1";
        };
      };
      config.krebs.xresources.resources.X = ''
        *.font:       ${config.lass.fonts.regular}
        *.boldFont:   ${config.lass.fonts.bold}
        *.italicFont: ${config.lass.fonts.italic}
      '';
    }
  ];

  users.extraUsers.mainUser.extraGroups = [ "audio" "video" ];

  time.timeZone = "Europe/Berlin";

  programs.ssh.agentTimeout = "10m";
  programs.ssh.startAgent = true;
  services.openssh.forwardX11 = true;

  services.printing = {
    enable = true;
    drivers = [
      pkgs.foomatic_filters
      pkgs.gutenprint
    ];
  };

  environment.systemPackages = with pkgs; [
    acpi
    ag
    bank
    cabal2nix
    dic
    dmenu
    gi
    gitAndTools.qgit
    git-preview
    gnome3.dconf
    lm_sensors
    mpv-poll
    much
    ncdu
    nix-index
    nix-repl
    nmap
    pavucontrol
    powertop
    push
    rxvt_unicode_with-plugins
    slock
    sxiv
    taskwarrior
    termite
    thesauron
    timewarrior
    xclip
    xephyrify
    xorg.xbacklight
    xorg.xhost
    xsel
    youtube-tools
    yt-next
    zathura
  ];

  fonts.fonts = with pkgs; [
    hack-font
    hasklig
    symbola
    xlibs.fontschumachermisc
  ];

  #lass.xserver.enable = true;
  services.xserver = {
    enable = true;
    layout = "us";
    display = mkForce 0;
    xkbModel = "evdev";
    xkbVariant = "altgr-intl";
    xkbOptions = "caps:backspace";
    displayManager.lightdm.enable = true;
    windowManager.default = "xmonad";
    windowManager.session = [{
      name = "xmonad";
      start = ''
        ${pkgs.xorg.xhost}/bin/xhost +LOCAL:
        ${pkgs.systemd}/bin/systemctl --user start xmonad
        exec ${pkgs.coreutils}/bin/sleep infinity
      '';
    }];
  };

  systemd.user.services.xmonad = {
    environment = {
      DISPLAY = ":${toString config.services.xserver.display}";
      RXVT_SOCKET = "%t/urxvtd-socket";
      XMONAD_DATA_DIR = "/tmp";
    };
    serviceConfig = {
      SyslogIdentifier = "xmonad";
      ExecStart = "${xmonad-lass}/bin/xmonad";
      ExecStop = "${xmonad-lass}/bin/xmonad --shutdown";
    };
    restartIfChanged = false;
  };

  krebs.xresources.enable = true;
  lass.screenlock.enable = true;
}
