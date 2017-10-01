{ config, pkgs, ... }:
with import <stockholm/lib>;
let
  user = config.krebs.build.user;
in {
  imports = [
    ./mpv.nix
    ./power-action.nix
    ./screenlock.nix
    ./copyq.nix
    ./xresources.nix
    ./livestream.nix
    ./dns-stuff.nix
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
      options.lass.myFont = mkOption {
        type = types.str;
        default = "-schumacher-clean-*-*-*-*-*-*-*-*-*-*-iso10646-1";
      };
    }
  ];

  users.extraUsers.mainUser.extraGroups = [ "audio" "video" ];

  time.timeZone = "Europe/Berlin";

  programs.ssh.startAgent = false;
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
    dic
    dmenu
    gi
    gitAndTools.qgit
    lm_sensors
    haskellPackages.hledger
    much
    ncdu
    nix-repl
    nmap
    pavucontrol
    powertop
    push
    slock
    sxiv
    xclip
    xorg.xbacklight
    xorg.xhost
    xsel
    zathura

    mpv-poll
    yt-next

    youtube-tools

    rxvt_unicode
    termite
  ];

  fonts.fonts = [
    pkgs.xlibs.fontschumachermisc
  ];

  services.xserver = {
    enable = true;

    desktopManager.xterm.enable = false;
    desktopManager.default = "none";
    displayManager.lightdm.enable = true;
    displayManager.lightdm.autoLogin = {
      enable = true;
      user = "lass";
    };
    windowManager.default = "xmonad";
    windowManager.session = [{
      name = "xmonad";
      start = ''
        ${pkgs.xorg.xhost}/bin/xhost +LOCAL:
        ${pkgs.xmonad-lass}/bin/xmonad &
        waitPID=$!
      '';
    }];

    layout = "us";
    xkbModel = "evdev";
    xkbVariant = "altgr-intl";
    xkbOptions = "caps:backspace";
  };

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
  '';

  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    accelFactor = "0.035";
  };

  services.urxvtd.enable = true;
}
