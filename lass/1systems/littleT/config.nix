with import <stockholm/lib>;
{ config, pkgs, ... }:

{
  imports = [
    <stockholm/lass>

    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/blue-host.nix>
    <stockholm/lass/2configs/syncthing.nix>
  ];

  networking.networkmanager.enable = true;
  networking.wireless.enable = mkForce false;
  time.timeZone = "Europe/Berlin";

  hardware.trackpoint = {
    enable = true;
    sensitivity = 220;
    speed = 0;
    emulateWheel = true;
  };

  services.logind.extraConfig = ''
    HandleLidSwitch=ignore
  '';

  krebs.build.host = config.krebs.hosts.littleT;
}
