{ config, pkgs, ... }:
let
  shack-ip = config.krebs.build.host.nets.shack.ip4.addr;
in
{
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    <stockholm/krebs/2configs/collectd-base.nix>
    <stockholm/krebs/2configs/stats/wolf-client.nix>
    <stockholm/krebs/2configs/save-diskspace.nix>

    <stockholm/krebs/2configs/graphite.nix>
    <stockholm/krebs/2configs/buildbot-krebs.nix>
    <stockholm/krebs/2configs/binary-cache/prism.nix>

    <stockholm/krebs/2configs/shack/worlddomination.nix>
    <stockholm/krebs/2configs/shack/drivedroid.nix>
    # <stockholm/krebs/2configs/shack/nix-cacher.nix>
    <stockholm/krebs/2configs/shack/mqtt_sub.nix>
    <stockholm/krebs/2configs/shack/muell_caller.nix>
    <stockholm/krebs/2configs/shack/radioactive.nix>
    <stockholm/krebs/2configs/shack/share.nix>

  ];
  # use your own binary cache, fallback use cache.nixos.org (which is used by
  # apt-cacher-ng in first place)

  services.influxdb.enable = true;

  # local discovery in shackspace
  nixpkgs.config.packageOverrides = pkgs: { tinc = pkgs.tinc_pre; };
  krebs.tinc.retiolum.extraConfig = "TCPOnly = yes";
  services.grafana = {
    enable = true;
    addr = "0.0.0.0";
    users.allowSignUp = true;
    users.allowOrgCreate = true;
    users.autoAssignOrg = true;
    auth.anonymous.enable = true;
    security = import <secrets/grafana_security.nix>;
  };

  nix = {
    # use the up to date prism cache
    binaryCaches = [
      "https://cache.nixos.org/"
    ];
    binaryCachePublicKeys = [
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
    ];
  };

  networking = {
    firewall.enable = false;
    firewall.allowedTCPPorts = [ 8088 8086 8083 ];
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
  # fallout of ipv6calypse
  networking.extraHosts = ''
    hass.shack    10.42.2.191
    heidi.shack   10.42.2.135
  '';

  users.extraUsers.root.openssh.authorizedKeys.keys = [
    config.krebs.users.ulrich.pubkey
    config.krebs.users.makefu-omo.pubkey
  ];

  time.timeZone = "Europe/Berlin";
  sound.enable = false;
}
