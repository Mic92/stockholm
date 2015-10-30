{ config, lib, pkgs, ... }:

with lib;

{
  imports = [
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    ../2configs/collectd-base.nix
  ];

  krebs.build.host = config.krebs.hosts.wolf;
  # TODO rename shared user to "krebs"
  krebs.build.user = config.krebs.users.shared;
  krebs.build.target = "wolf";

  krebs.enable = true;
  krebs.retiolum = {
    enable = true;
    connectTo = [
      # TODO remove connectTo cd, this was only used for bootstrapping
      "cd"
      "gum"
      "pigstarter"
    ];
  };

  krebs.build.source = {
    git.nixpkgs = {
      url = https://github.com/NixOS/nixpkgs;
      rev = "e916273209560b302ab231606babf5ce1c481f08";
    };
    dir.secrets = {
      host = config.krebs.current.host;
      path = "${getEnv "HOME"}/secrets/krebs/wolf";
    };
    dir.stockholm = {
      host = config.krebs.current.host;
      path = "${getEnv "HOME"}/stockholm";
    };
  };

  networking.hostName = config.krebs.build.host.name;

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
    { device = "/dev/disk/by-label/swap"; }
  ];

  nix.maxJobs = 1;
  nix.trustedBinaryCaches = [
    "https://cache.nixos.org"
    "http://cache.nixos.org"
    "http://hydra.nixos.org"
  ];
  nix.useChroot = true;

  nixpkgs.config.packageOverrides = pkgs: {
    nano = pkgs.vim;
  };

  environment.systemPackages = with pkgs; [
    git
    rxvt_unicode.terminfo
  ];

  time.timeZone = "Europe/Berlin";

  programs.ssh.startAgent = false;

  services.openssh = {
    enable = true;
    hostKeys = [
      { type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
    ];
  };
  services.cron.enable = false;
  services.nscd.enable = false;
  services.ntp.enable = false;

  users.mutableUsers = false;
  users.extraUsers.root.openssh.authorizedKeys.keys = [
    # TODO
    config.krebs.users.lass.pubkey
    config.krebs.users.makefu.pubkey
    config.krebs.users.tv.pubkey
  ];

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";
}
