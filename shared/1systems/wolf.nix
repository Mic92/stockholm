{ config, lib, pkgs, ... }:

let
  shack-ip = config.krebs.build.host.nets.shack.ip4.addr;
  internal-ip = config.krebs.build.host.nets.retiolum.ip4.addr;
in
{
  imports = [
    ../.
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    ../2configs/collectd-base.nix
    ../2configs/shack-nix-cacher.nix
    ../2configs/shack-drivedroid.nix
    ../2configs/shared-buildbot.nix
    ../2configs/cgit-mirror.nix
    ../2configs/repo-sync.nix
    ../2configs/graphite.nix
  ];
  # use your own binary cache, fallback use cache.nixos.org (which is used by
  # apt-cacher-ng in first place)

  # local discovery in shackspace
  nixpkgs.config.packageOverrides = pkgs: { tinc = pkgs.tinc_pre; };
  krebs.tinc.retiolum.extraConfig = "TCPOnly = yes";
  services.grafana = {
    enable = true;
    addr = "0.0.0.0";
    extraOptions = { "AUTH_ANONYMOUS_ENABLED" = "true"; };
    users.allowSignUp = true;
    users.allowOrgCreate = true;
    users.autoAssignOrg = true;
    security = import <secrets/grafana_security.nix>;
  };

  nix.binaryCaches = [ "http://localhost:3142/nixos" "https://cache.nixos.org" ];

  networking = {
    firewall.enable = false;
    interfaces.enp0s3.ip4 = [{
      address = shack-ip;
      prefixLength = 20;
    }];

    defaultGateway = "10.42.0.1";
    nameservers = [ "10.42.0.100" "10.42.0.200" ];
  };

  #####################
  # uninteresting stuff
  #####################
  krebs.build.host = config.krebs.hosts.wolf;

  boot.kernel.sysctl = {
    # Enable IPv6 Privacy Extensions
    "net.ipv6.conf.all.use_tempaddr" = 2;
    "net.ipv6.conf.default.use_tempaddr" = 2;
  };

  boot.initrd.availableKernelModules = [
    "ata_piix" "uhci_hcd" "ehci_pci" "virtio_pci" "virtio_blk"
  ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda";

  fileSystems."/" = { device = "/dev/disk/by-label/nixos"; fsType = "ext4"; };

  swapDevices = [
    { device = "/dev/disk/by-label/swap";  }
  ];

  time.timeZone = "Europe/Berlin";
}
