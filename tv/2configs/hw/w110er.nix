with import <stockholm/lib>;
{ pkgs, ... }:

{
  imports = [
    ../smartd.nix
    {
      # nvidia doesn't build despite
      #  https://github.com/NixOS/nixpkgs/issues/33284
      #hardware.bumblebee.enable = true;
      #hardware.bumblebee.group = "video";
      #hardware.enableRedistributableFirmware= true;
      #krebs.nixpkgs.allowUnfreePredicate = pkg:
      #  hasPrefix "nvidia-x11-" pkg.name ||
      #  hasPrefix "nvidia-persistenced-" pkg.name ||
      #  hasPrefix "nvidia-settings-" pkg.name;
    }
  ];

  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
  '';

  boot.initrd.availableKernelModules = [ "ahci" ];
  boot.kernelModules = [ "kvm-intel" ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];

  networking.wireless.enable = true;

  nix = {
    buildCores = 4;
    maxJobs = 4;
    daemonIONiceLevel = 1;
    daemonNiceLevel = 1;
  };

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
}
