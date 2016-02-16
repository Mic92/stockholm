{ config, lib, pkgs, ... }:

{
  krebs.hosts.xu-qemu0 = {
    cores = 1;
    ssh.privkey.path = <secrets/ssh.id_ed25519>;
    # cannot define ssh.pubkey without at least one addr or alias
    #ssh.pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFe51rD0ZqlMXNi/YpapnRzvdzCjI0icmxfCyBLSKG04";
  };
  krebs.build.host = config.krebs.hosts.xu-qemu0;

  imports = [
    ../.
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
  ];

  boot.loader.grub.device = "/dev/sda";

  fileSystems = {
    "/boot" = {
      device = "/dev/sda1";
    };
    "/" = {
      device = "/dev/sda2";
      fsType = "btrfs";
    };
  };
}
