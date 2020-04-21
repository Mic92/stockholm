# Configuration for the desktop environment

{ config, lib, pkgs, ... }:
{
  # Configure basic X-server stuff:
  services.xserver = {
    enable = true;
    xkbOptions = "caps:super";
    exportConfiguration = true;

    displayManager.lightdm.enable = true;
  };

  # Configure fonts
  fonts = {
    fonts = with pkgs; [
      corefonts
      font-awesome-ttf
      noto-fonts-cjk
      noto-fonts-emoji
      powerline-fonts
      helvetica-neue-lt-std
    ];
  };
}
