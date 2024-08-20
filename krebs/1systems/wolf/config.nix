{ config, pkgs, ... }:
let
  shack-ip = config.krebs.build.host.nets.shack.ip4.addr;
  ext-if = "et0";
  external-mac = "52:54:b0:0b:af:fe";

in
{
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>

    <stockholm/krebs/2configs/binary-cache/nixos.nix>
    <stockholm/krebs/2configs/binary-cache/prism.nix>

    #### shackspace services
    <stockholm/krebs/2configs/shack/share.nix> # wolf.shack

    # gitlab runner
    <stockholm/krebs/2configs/shack/gitlab-runner.nix>
    # misc
    <stockholm/krebs/2configs/shack/ssh-keys.nix>
    <stockholm/krebs/2configs/save-diskspace.nix>
    <stockholm/krebs/2configs/shack/prometheus/node.nix>

  ];
  # use your own binary cache, fallback use cache.nixos.org (which is used by
  # apt-cacher-ng in first place)

  # local discovery in shackspace
  nixpkgs.config.packageOverrides = pkgs: { tinc = pkgs.tinc_pre; };
  krebs.tinc.retiolum.extraConfig = "TCPOnly = yes";


  networking = {
    firewall.enable = false;
    firewall.allowedTCPPorts = [ 8088 8086 8083 ];
    interfaces."${ext-if}".ipv4.addresses = [
      {
        address = shack-ip;
        prefixLength = 20;
      }
    ];

    defaultGateway = "10.42.0.1";
    nameservers = [ "10.42.0.100" "10.42.0.200" ];
  };

  #####################
  # uninteresting stuff
  #####################
  krebs.build.host = config.krebs.hosts.wolf;
  krebs.hosts.wolf.ssh.privkey.path = "${config.krebs.secret.directory}/ssh.id_ed25519";

  boot.initrd.availableKernelModules = [
    "ata_piix" "uhci_hcd" "ehci_pci" "virtio_pci" "virtio_blk"
  ];
  boot.kernelModules = [ ];
  boot.extraModulePackages = [ ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/vda";

  # without it `/nix/store` is not added grub paths
  boot.loader.grub.copyKernels = true;

  fileSystems."/" = { device = "/dev/disk/by-label/nixos"; fsType = "ext4"; };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="${external-mac}", NAME="${ext-if}"
  '';

  time.timeZone = "Europe/Berlin";

  # avahi
  services.avahi = {
    enable = true;
    wideArea = false;
  };

  environment.systemPackages = [ pkgs.avahi ];

}
