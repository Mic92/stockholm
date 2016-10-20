{ config, lib, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;
  inherit (import <stockholm/lib>) genid;

in {
  virtualisation.libvirtd.enable = true;

  users.extraUsers = {
    libvirt = {
      uid = genid "libvirt";
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
