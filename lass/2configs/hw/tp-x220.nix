{ config, lib, pkgs, ... }:

with import <stockholm/lib>;
{
  imports = [
    ../smartd.nix
  ];
  networking.wireless.enable = lib.mkDefault true;

  hardware.enableAllFirmware = true;
  nixpkgs.config.allowUnfree = true;

  hardware.cpu.intel.updateMicrocode = true;

  zramSwap.enable = true;
  zramSwap.numDevices = 2;

  hardware.trackpoint = {
    enable = true;
    sensitivity = 220;
    speed = 0;
    emulateWheel = true;
  };

  services.tlp.enable = true;
  services.tlp.extraConfig = ''
    # BUG: http://linrunner.de/en/tlp/docs/tlp-faq.html#erratic-battery
    #START_CHARGE_THRESH_BAT0=80
    STOP_CHARGE_THRESH_BAT0=95

    CPU_SCALING_GOVERNOR_ON_AC=performance
    CPU_SCALING_GOVERNOR_ON_BAT=ondemand
    CPU_MIN_PERF_ON_AC=0
    CPU_MAX_PERF_ON_AC=100
    CPU_MIN_PERF_ON_BAT=0
    CPU_MAX_PERF_ON_BAT=30
  '';

  boot = {
    kernelModules = [ "kvm-intel" "acpi_call" "tpm-rng" ];
    extraModulePackages = [ config.boot.kernelPackages.tp_smapi ];
    kernelParams = [ "acpi_backlight=none" ];
  };

  hardware.opengl.extraPackages = [
    pkgs.vaapiIntel
    pkgs.vaapiVdpau
  ];

  services.xserver = {
    videoDriver = "intel";
    deviceSection = ''
      Option "AccelMethod" "sna"
    '';
  };

  security.rngd.enable = true;
}
