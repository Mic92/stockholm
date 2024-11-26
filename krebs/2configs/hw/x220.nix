{ lib, pkgs, ... }:

{
  networking.wireless.enable = lib.mkDefault true;

  hardware.enableRedistributableFirmware = true;

  hardware.cpu.intel.updateMicrocode = true;

  hardware.opengl.enable = true;

  services.tlp.enable = true;

  boot = {
    kernelModules = [ "kvm-intel" "acpi_call" "tpm-rng" ];
    kernelParams = [ "acpi_backlight=none" ];
  };

  hardware.opengl.extraPackages = [
    pkgs.vaapiIntel
    pkgs.vaapiVdpau
  ];

  services.xserver = {
    videoDriver = "intel";
  };
}
