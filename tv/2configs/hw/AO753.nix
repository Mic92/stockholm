{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    ../smartd.nix
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

  krebs.nixpkgs.allowUnfreePredicate = pkg: hasPrefix "broadcom-sta-" pkg.name;
}
