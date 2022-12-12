with import ./lib;
{ config, pkgs, ... }: {
  imports = [
    ../smartd.nix

    {
      nix.buildCores = 2;
      nix.maxJobs = 2;
    }
    (if lib.versionAtLeast (lib.versions.majorMinor lib.version) "21.11" then {
      nix.daemonCPUSchedPolicy = "batch";
      nix.daemonIOSchedPriority = 1;
    } else {
      nix.daemonIONiceLevel = 1;
      nix.daemonNiceLevel = 1;
    })
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
