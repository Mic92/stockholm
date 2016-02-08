{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;
in {
  environment.systemPackages = with pkgs; [
    electrum
  ];

  users.extraUsers = {
    bitcoin = {
      name = "bitcoin";
      description = "user for bitcoin stuff";
      home = "/home/bitcoin";
      useDefaultShell = true;
      createHome = true;
    };
  };
  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(bitcoin) NOPASSWD: ALL
  '';
}
