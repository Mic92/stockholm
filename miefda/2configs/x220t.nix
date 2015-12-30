{ config, lib, pkgs, ... }:

with lib;
{

  services.xserver = {
	xkbVariant = "altgr-intl";
    videoDriver = "intel";
   # vaapiDrivers = [ pkgs.vaapiIntel pkgs.vaapiVdpau ];
    deviceSection = ''
      Option "AccelMethod" "sna"
    '';
  };



  services.xserver.displayManager.sessionCommands =''
  xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 8 1
  xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 8 2
  xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 6 7 4 5
  # xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Timeout" 8 200
  '';

  hardware.bluetooth.enable = true;


}
