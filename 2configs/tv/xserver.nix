{ config, pkgs, ... }:

{
  imports = [
    ../../2configs/tv/urxvt.nix # TODO via xserver
  ];

  services.xserver.enable = true;


  #fonts.enableFontConfig = true;
  #fonts.enableFontDir = true;
  fonts.fonts = [
    pkgs.xlibs.fontschumachermisc
  ];
  #services.xfs.enable = true;
  #services.xserver.useXFS = "unix/:7100";

  services.xserver.displayManager.desktopManagerHandlesLidAndPower = true;

  #services.xserver.display = 11;
  #services.xserver.tty = 11;
  # services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";

  #services.xserver.multitouch.enable = true;

  services.xserver.windowManager.xmonad.extraPackages = hspkgs: with hspkgs; [
    X11-xshape
  ];
  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.windowManager.default = "xmonad";
  services.xserver.desktopManager.default = "none";
  services.xserver.desktopManager.xterm.enable = false;

  services.xserver.displayManager.slim.enable = true;
  #services.xserver.displayManager.auto.enable = true;
  #services.xserver.displayManager.auto.user = "tv";
  #services.xserver.displayManager.job.logsXsession = true;
}
