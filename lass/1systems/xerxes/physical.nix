{ pkgs, lib, ... }:
{
  imports = [
    ./config.nix
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];

  boot.zfs.enableUnstable = true;
  boot.loader.grub = {
    enable = true;
    device = "/dev/sda";
    efiSupport = true;
  };
  boot.loader.efi.canTouchEfiVariables = true;

  boot.blacklistedKernelModules = [
    "sdhci_pci"
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usbhid" "sd_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.initrd.luks.devices.crypted.device = "/dev/sda3";
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.kernelParams = [
    "fbcon=rotate:1"
    "boot.shell_on_fail"
  ];

  fileSystems."/" = {
    device = "rpool/root";
    fsType = "zfs";
  };

  fileSystems."/home" = {
    device = "rpool/home";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/E749-784C";
    fsType = "vfat";
  };

  swapDevices = [ ];

  boot.extraModprobeConfig = ''
    options zfs zfs_arc_max=107374182
  '';

  nix.maxJobs = lib.mkDefault 4;

  networking.hostId = "9b0a74ac";
  networking.networkmanager.enable = true;

  hardware.opengl.enable = true;

  services.tlp.enable = true;
  services.tlp.extraConfig = ''
    CPU_SCALING_GOVERNOR_ON_AC=ondemand
    CPU_SCALING_GOVERNOR_ON_BAT=powersave
    CPU_MIN_PERF_ON_AC=0
    CPU_MAX_PERF_ON_AC=100
    CPU_MIN_PERF_ON_BAT=0
    CPU_MAX_PERF_ON_BAT=30
  '';

  services.logind.extraConfig = ''
    HandlePowerKey=suspend
    IdleAction=suspend
    IdleActionSec=300
  '';

  services.xserver = {
    videoDrivers = [ "intel" ];
    deviceSection = ''
      Option "TearFree" "true"
    '';
    displayManager.sessionCommands = ''
      echo nonono > /tmp/xxyy
      (sleep 2 && ${pkgs.xorg.xrandr}/bin/xrandr --output eDP1 --rotate right)
      (sleep 2 && ${pkgs.xorg.xinput}/bin/xinput set-prop 'Goodix Capacitive TouchScreen' 'Coordinate Transformation Matrix' 0 1 0 -1 0 1 0 0 1)
    '';
  };
}
