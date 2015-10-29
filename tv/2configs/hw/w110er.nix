{ pkgs, ... }:

{
  imports = [
    ../smartd.nix
  ];

  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
  '';

  boot.initrd.availableKernelModules = [ "ahci" ];
  boot.kernelModules = [ "kvm-intel" ];

  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

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

  services.xserver = {
    vaapiDrivers = [ pkgs.vaapiIntel ];
  };

  system.activationScripts.powertopTunables = ''
    echo 1 > /sys/module/snd_hda_intel/parameters/power_save
    echo 1500 > /proc/sys/vm/dirty_writeback_centisecs
    (cd /sys/bus/pci/devices
      for i in *; do
        echo auto > $i/power/control # defaults to 'on'
      done)
  '';
}
