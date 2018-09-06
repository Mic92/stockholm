{ config, pkgs, ... }:

{

  nixpkgs.config.steam.java = true;
  environment.systemPackages = with pkgs; [
    steam
  ];
  hardware.opengl.driSupport32Bit = true;

  #ports for inhome streaming
}
