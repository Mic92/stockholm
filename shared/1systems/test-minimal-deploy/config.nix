{ config, pkgs, lib, ... }:
{
  imports = [
    <stockholm/shared>
  ];
  krebs = {
    enable = true;
    build.user = config.krebs.users.shared;
    build.host = config.krebs.hosts.test-all-krebs-modules;
  };
  # just get the system to eval in nixos without errors
  boot.loader.grub.devices = ["/dev/sda"];
  fileSystems."/" = {
    device = "/dev/lol";
  };
}
