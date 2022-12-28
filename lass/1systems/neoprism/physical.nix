{ config, lib, pkgs, ... }:

{

  imports = [
    ./config.nix
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
  ];

  disko.devices = import ./disk.nix;
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.efiSupport = true;
  boot.loader.grub.devices = [ "/dev/nvme0n1" "/dev/nvme1n1" ];
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "sd_mod" ];
  boot.kernelModules = [ "kvm-amd" ];
  hardware.cpu.amd.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  # networking config
  boot.kernelParams = [ "net.ifnames=0" ];
  networking.bridges."ext-br".interfaces = [ "eth0" ];
  networking = {
    hostId = "2283aaae";
    defaultGateway = "95.217.192.1";
    defaultGateway6 = { address = "fe80::1"; interface = "ext-br"; };
    # Use google's public DNS server
    nameservers = [ "8.8.8.8" ];
    interfaces.ext-br.ipv4.addresses = [
      {
        address = "95.217.192.59";
        prefixLength = 26;
      }
    ];
    interfaces.ext-br.ipv6.addresses = [
      {
        address = "2a01:4f9:4a:4f1a::1";
        prefixLength = 64;
      }
    ];
  };

}
