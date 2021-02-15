{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

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

  # broadcom_sta is marked as broken for 5.9+
  # pkgs.linuxPackages_latest ist 5.9
  boot.kernelPackages = pkgs.linuxPackages_5_8;

  boot.extraModulePackages = [
    config.boot.kernelPackages.broadcom_sta
  ];

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

  krebs.nixpkgs.allowUnfreePredicate = pkg: packageName pkg == "broadcom-sta";

  tv.hw.screens.primary.width = 1366;
  tv.hw.screens.primary.height = 768;
}
