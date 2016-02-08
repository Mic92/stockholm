{ config, lib, pkgs, ... }:

with lib;
{
  hardware.enableAllFirmware = true;
  nixpkgs.config.allowUnfree = true;

  hardware.cpu.intel.updateMicrocode = true;

  zramSwap.enable = true;
  zramSwap.numDevices = 2;

  hardware.trackpoint = {
    enable = true;
    sensitivity = 220;
    speed = 220;
    emulateWheel = true;
    };


  services.tlp.enable = true;
  services.tlp.extraConfig = ''
  START_CHARGE_THRESH_BAT0=80
  '';
}
