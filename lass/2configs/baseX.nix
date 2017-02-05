{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;
in {
  imports = [
    ./xserver
    ./mpv.nix
    ./power-action.nix
    ./screenlock.nix
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
  #window manager stuff
    #haskellPackages.xmobar
    #haskellPackages.yeganesh
    #dmenu2
    #xlibs.fontschumachermisc
  ];

  #fonts.fonts = [
  #  pkgs.xlibs.fontschumachermisc
  #];

  #services.xserver = {
  #  enable = true;

  #  windowManager.xmonad.extraPackages = hspkgs: with hspkgs; [
  #    X11-xshape
  #  ];
  #  windowManager.xmonad.enable = true;
  #  windowManager.xmonad.enableContribAndExtras = true;
  #  windowManager.default = "xmonad";
  #  desktopManager.default = "none";
  #  desktopManager.xterm.enable = false;
  #  displayManager.slim.enable = true;
  #  displayManager.auto.enable = true;
  #  displayManager.auto.user = mainUser.name;

  #  layout = "us";
  #  xkbModel = "evdev";
  #  xkbVariant = "altgr-intl";
  #  xkbOptions = "caps:backspace";
  #};

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
  '';

  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    accelFactor = "0.035";
  };
}
