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
    {
      hardware.pulseaudio = {
        enable = true;
        systemWide = true;
      };
    }
    {
      krebs.per-user.lass.packages = [
        pkgs.sshuttle
      ];
      security.sudo.extraConfig = ''
        lass ALL= (root) NOPASSWD:SETENV: ${pkgs.sshuttle}/bin/.sshuttle-wrapped
      '';
    }
  ];

  users.extraUsers.mainUser.extraGroups = [ "audio" "video" ];

  time.timeZone = "Europe/Berlin";

  virtualisation.libvirtd.enable = true;

  programs.ssh.startAgent = false;

  security.setuidPrograms = [ "slock" ];

  services.printing = {
    enable = true;
    drivers = [ pkgs.foomatic_filters ];
  };

  environment.systemPackages = with pkgs; [

    acpi
    dic
    dmenu
    gitAndTools.qgit
    lm_sensors
    much
    ncdu
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
  ];

  fonts.fonts = [
    pkgs.xlibs.fontschumachermisc
  ];

  services.xserver = {
    enable = true;

    desktopManager.xterm.enable = false;
    displayManager.slim.enable = true;
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
