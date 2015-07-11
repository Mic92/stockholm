{ config, pkgs, ... }:

{
  imports = [
    ../../2configs/tv/smartd.nix
  ];

  boot.loader.grub = {
    device = "/dev/sda";
    splashImage = null;
  };

  boot.initrd.availableKernelModules = [
    "ahci"
  ];

  boot.kernelModules = [
    "kvm-intel"
    "wl"
  ];

  boot.extraModulePackages = [
    config.boot.kernelPackages.broadcom_sta
  ];

  networking.wireless.enable = true;

  services.logind.extraConfig = ''
    HandleHibernateKey=ignore
    HandleLidSwitch=ignore
    HandlePowerKey=ignore
    HandleSuspendKey=ignore
  '';

  nixpkgs.config = {
    allowUnfree = false;
    allowUnfreePredicate = (x: pkgs.lib.hasPrefix "broadcom-sta-" x.name);
  };
}
