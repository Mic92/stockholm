{ config, lib, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;

in {
  virtualisation.libvirtd.enable = true;

  users.extraUsers = {
    libvirt = {
      uid = lib.genid "libvirt";
      description = "user for running libvirt stuff";
      home = "/home/libvirt";
      useDefaultShell = true;
      extraGroups = [ "libvirtd" "audio" ];
      createHome = true;
    };
  };
  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(libvirt) NOPASSWD: ALL
  '';
}
