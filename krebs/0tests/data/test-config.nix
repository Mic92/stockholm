{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/krebs>
    <nixpkgs/nixos/modules/virtualisation/qemu-vm.nix>
    <nixpkgs/nixos/modules/testing/test-instrumentation.nix>
  ];

  krebs.hosts.minimal = {
    secure = false;
  };

  boot.loader.grub.enable = false;
  boot.loader.systemd-boot.enable = true;

  krebs.build = {
    host = config.krebs.hosts.minimal;
    user = config.krebs.users.krebs;
  };
}
