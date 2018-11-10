{ config, lib, pkgs, ... }:

{

  imports = [
    ./config.nix
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "sd_mod" ];
  boot.kernelModules = [ "kvm-intel" ];

  fileSystems."/" = {
    device = "rpool/root/nixos";
    fsType = "zfs";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/d155d6ff-8e89-4876-a9e7-d1b7ba6a4804";
    fsType = "ext4";
  };

  fileSystems."/srv/http" = {
    device = "tank/srv-http";
    fsType = "zfs";
  };

  fileSystems."/var/lib/containers" = {
    device = "tank/containers";
    fsType = "zfs";
  };

  fileSystems."/home" = {
    device = "tank/home";
    fsType = "zfs";
  };

  nix.maxJobs = lib.mkDefault 8;
  powerManagement.cpuFreqGovernor = lib.mkDefault "powersave";

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.devices = [ "/dev/sda" "/dev/sdb" ];

  boot.kernelParams = [ "net.ifnames=0" ];
  networking = {
    hostId = "2283aaae";
    defaultGateway = "95.216.1.129";
    # Use google's public DNS server
    nameservers = [ "8.8.8.8" ];
    interfaces.eth0 = {
      ipAddress = "95.216.1.150";
      prefixLength = 26;
    };
  };
}
