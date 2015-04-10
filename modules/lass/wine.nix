{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    wineUnstable
  ];
  users.extraUsers = {
    wine = {
      name = "wine";
      description = "user for running wine";
      home = "/home/wine";
      useDefaultShell = true;
      extraGroups = [];
      createHome = true;
    };
  };
  security.sudo.extraConfig = ''
    lass ALL=(wine) NOPASSWD: ALL
  '';
}
