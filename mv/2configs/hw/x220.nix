{ config, pkgs, ... }:

{
  imports = [
    ../smartd.nix
  ];

  boot.initrd.availableKernelModules = [
    "aesni-intel"
    "ahci"
    "fbcon"
    "i915"
  ];
  boot.kernelModules = [
    "kvm-intel"
    "msr"
    "tp-smapi"
  ];

  boot.extraModulePackages = [
    config.boot.kernelPackages.tp_smapi
  ];

  # disabled for fbcon and i915 to kick in or to disable the kernelParams
  # XXX: investigate
  boot.vesa = false;

  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.wireless.enable = true;

  #hardware.enableAllFirmware = true;
  #nixpkgs.config.allowUnfree = true;
  #zramSwap.enable = true;
  #zramSwap.numDevices = 2;

  hardware.trackpoint = {
    enable = true;
    sensitivity = 220;
    speed = 0;
    emulateWheel = true;
  };

  services.tlp.enable = true;
  services.tlp.extraConfig = ''
    START_CHARGE_THRESH_BAT0=80
  '';

  nix = {
    buildCores = 2;
    maxJobs = 2;
    daemonIONiceLevel = 1;
    daemonNiceLevel = 1;
  };

  services.logind.extraConfig = ''
    HandleHibernateKey=ignore
    HandleLidSwitch=ignore
    HandlePowerKey=ignore
    HandleSuspendKey=ignore
  '';

  services.xserver = {
    videoDriver = "intel";
    #vaapiDrivers = [ pkgs.vaapiIntel pkgs.vaapiVdpau ];
    #deviceSection = ''
    #  Option "AccelMethod" "sna"
    #'';
  };

  #services.xserver.displayManager.sessionCommands =''
  #  xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 8 1
  #  xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 8 2
  #  xinput set-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 6 7 4 5
  #'';
}
