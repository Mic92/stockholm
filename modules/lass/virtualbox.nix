{ config, pkgs, ... }:

{
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
}
