{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;
in {
  imports = [
    ./base.nix
    #./urxvt.nix
    ./xserver
  ];

  users.extraUsers.mainUser.extraGroups = [ "audio" ];

  time.timeZone = "Europe/Berlin";

  virtualisation.libvirtd.enable = true;

  hardware.pulseaudio = {
    enable = true;
    systemWide = true;
  };

  programs.ssh.startAgent = false;

  security.setuidPrograms = [ "slock" ];

  services.printing = {
    enable = true;
    drivers = [ pkgs.foomatic_filters ];
  };

  environment.systemPackages = with pkgs; [

    powertop
    sxiv
    much
    push
    zathura

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

}
