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

  # without it `/nix/store` is not added grub paths
  boot.loader.grub.copyKernels = true;

  fileSystems."/" = { device = "/dev/disk/by-label/nixos"; fsType = "ext4"; };

  users.extraUsers.root.openssh.authorizedKeys.keys = [
    config.krebs.users."0x4A6F".pubkey
    config.krebs.users.ulrich.pubkey
    config.krebs.users.raute.pubkey
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAAEAQDb9NPa2Hf51afcG1H13UPbE5E02J8aC9a1sGCRls592wAVlQbmojYR1jWDPA2m32Bsyv0ztqi81zDyndWWZPQVJVBk00VjYBcgk6D5ifqoAuWLzfuHJPWZGOvBf/U74/LNFNUkj1ywjneK7HYTRPXrRBBfBSQNmQzkvue7s599L2vdueZKyjNsMpx2m6nm2SchaMuDskSQut/168JgU1l4M8BeT68Bo4WdelhBYnhSI1a59FGkgdu2SCjyighLQRy2sOH3ksnkHWENPkA+wwQOlKl7R3DsEybrNd4NU9FSwFDyDmdhfv5gJp8UGSFdjAwx43+8zM5t5ruZ25J0LnVb0PuTuRA00UsW83MkLxFpDQLrQV08tlsY6iGrqxP67C3VJ6t4v6oTp7/vaRLhEFc1PhOLh+sZ18o8MLO+e2rGmHGHQnSKfBOLUvDMGa4jb01XBGjdnIXLOkVo79YR5jZn7jJb2gTZ95OD6bWSDADoURSuwuLa7kh4ti1ItAKuhkIvbuky3rRVvQEc92kJ6aNUswIUXJa0K2ibbIY6ycKAA3Ljksl3Mm9KzOn6yc/i/lSF+SOrTGhabPJigKkIoqKIwnV5IU3gkfsxPQJOBMPqHDGAOeYQe3WpWedEPYuhQEczw4exMb9TkNE96F71PzuQPJDl5sPAWyPLeMKpy5XbfRiF2by4nxN3ZIQvjtoyVkjNV+qM0q0yKBzLxuRAEQOZ2yCEaBudZQkQiwHD97H2vu4SRQ/2aOie1XiOnmdbQRDZSO3BsoDK569K1w+gDfSnqY7zVUMj6tw+uKx6Gstck5lbvYMtdWKsfPv/pDM8eyIVFLL93dKTX+ertcQj6xDwLfOiNubE5ayFXhYkjwImV6NgfBuq+3hLK0URP2rPlOZbbZTQ0WlKD6CCRZPMSZCU9oD2zYfqpvRArBUcdkAwGePezORkfJQLE6mYEJp6pdFkJ/IeFLbO6M0lZVlfnpzAC9kjjkMCRofZUETcFSppyTImCbgo3+ok59/PkNU5oavBXyW80ue2tWHr08HX/QALNte3UITmIIlU6SFMCPMWJqadK1eDPWfJ4H4iDXRNn3D5wqN++iMloKvpaj0wieqXLY4+YfvNTNr177OU48GEWW8DnoEkbpwsCbjPxznGDQhdDqdYyMY/fDgRQReKITvKYGHRzesGysw5cKsp9LEfXD0R6WE2TeiiENla5AWzTgXJB0AyZEcOiIfqOgT9Nr9S8q5gc/BdA7P+jhGGJgEHhV3dVlfIZ7pmZc27Yu7UTQ0lbAKWqcMSTOdne+QL6ILzbvLrQwdvax4tQdm5opfU16SrOox1AMwAbkdq84z6uJqYVx3cUXfMJgTyDNrVv3or root@plattenschwein" # for backup
    "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC1Lx5MKtVjB/Ef6LpEiIAgVwY5xKQFdHuLQR+odQO4cAgxj1QaIXGN0moixY52DebVQhAtiCNiFZ83uJyOj8kmu30yuXwtSOQeqziA859qMJKZ4ZcYdKvbXwnf2Chm5Ck/0FvtpjTWHIZAogwP1wQto/lcqHOjrTAnZeJfQuHTswYUSnmUU5zdsEZ9HidDPUc2Gv0wkBNd+KMQyOZl0HkaxHWvn0h4KK4hYZisOpeTfXJxD87bo+Eg4LL2vvnHW6dF6Ygrbd/0XRMsRRI8OAReVBUoJn7IE1wwAl/FpblNmhaF9hlL7g7hR1ADvaWMMw0e8SSzW6Y+oIa8qFQL6wR1 gitlab-builder" # for being deployed by gitlab ci
  ];

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="${external-mac}", NAME="${ext-if}"
  '';

  time.timeZone = "Europe/Berlin";
  sound.enable = false;

  # avahi
  services.avahi = {
    enable = true;
    wideArea = false;
  };
  environment.systemPackages = [ pkgs.avahi ];

}
