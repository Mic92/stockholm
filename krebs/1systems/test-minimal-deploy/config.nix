{ config, pkgs, lib, ... }:
{
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>
  ];
  krebs = {
    enable = true;
    build.user = config.krebs.users.krebs;
    build.host = config.krebs.hosts.test-all-krebs-modules;
  };
  # just get the system to eval in nixos without errors
  boot.loader.grub.devices = ["/dev/sda"];
  fileSystems."/" = {
    device = "/dev/lol";
  };
}
