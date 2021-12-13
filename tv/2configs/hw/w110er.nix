{ pkgs, ... }: let
  lib = import <stockholm/lib>;
in {
  imports = [
    ../smartd.nix
    {
      # nvidia doesn't build despite
      #  https://github.com/NixOS/nixpkgs/issues/33284
      #hardware.bumblebee.enable = true;
      #hardware.bumblebee.group = "video";
      #hardware.enableRedistributableFirmware= true;
      #krebs.nixpkgs.allowUnfreePredicate = pkg: any (eq (packageName pkg)) [
      #  "nvidia-x11"
      #  "nvidia-persistenced"
      #  "nvidia-settings"
      #];
    }

    {
      nix.buildCores = 4;
      nix.maxJobs = 4;
    }
    (if lib.versionAtLeast (lib.versions.majorMinor lib.version) "21.11" then {
      nix.daemonCPUSchedPolicy = "batch";
      nix.daemonIOSchedPriority = 1;
    } else {
      nix.daemonIONiceLevel = 1;
      nix.daemonNiceLevel = 1;
    })
  ];

  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
  '';

  boot.initrd.availableKernelModules = [ "ahci" ];
  boot.kernelModules = [ "kvm-intel" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.enableRedistributableFirmware = true;

  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];

  networking.wireless.enable = true;

  services.logind.extraConfig = ''
    HandleHibernateKey=ignore
    HandleLidSwitch=ignore
    HandlePowerKey=ignore
    HandleSuspendKey=ignore
  '';

  system.activationScripts.powertopTunables = ''
    echo 1 > /sys/module/snd_hda_intel/parameters/power_save
    echo 1500 > /proc/sys/vm/dirty_writeback_centisecs
    (cd /sys/bus/pci/devices
      for i in *; do
        echo auto > $i/power/control # defaults to 'on'
      done)
  '';

  services.xserver = {
    videoDriver = "intel";
  };

  tv.hw.screens.primary.width = 1366;
  tv.hw.screens.primary.height = 768;
}
