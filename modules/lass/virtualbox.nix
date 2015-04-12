{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;

in {
  services.virtualboxHost.enable = true;

  users.extraUsers = {
    virtual = {
      name = "virtual";
      description = "user for running VirtualBox";
      home = "/home/virtual";
      useDefaultShell = true;
      extraGroups = [ "vboxusers" ];
      createHome = true;
    };
  };
  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(virtual) NOPASSWD: ALL
  '';
}
