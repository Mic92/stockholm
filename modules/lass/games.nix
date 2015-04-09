{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    dwarf_fortress
  ];

  users.extraUsers = {
    games = {
      name = "games";
      description = "user playing games";
      home = "/home/games";
      extraGroups = [ "audio" ];
      createHome = true;
      useDefaultShell = true;
    };
  };

  security.sudo.extraConfig = ''
    lass ALL=(games) NOPASSWD: ALL
  '';
}
