{ config, pkgs, ... }:

{
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
}
