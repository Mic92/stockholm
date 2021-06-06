{ config, pkgs, ... }:

let
  mainUser = config.users.extraUsers.mainUser;
in {

  users.extraUsers = {
    bch = {
      name = "bch";
      description = "user for bch stuff";
      home = "/home/bch";
      useDefaultShell = true;
      createHome = true;
      packages = [ pkgs.electron-cash ];
      isNormalUser = true;
    };
    bitcoin = {
      name = "bitcoin";
      description = "user for bitcoin stuff";
      home = "/home/bitcoin";
      useDefaultShell = true;
      createHome = true;
      packages = [ pkgs.electrum ];
      isNormalUser = true;
    };
    monero = {
      name = "monero";
      description = "user for monero stuff";
      home = "/home/monero";
      useDefaultShell = true;
      createHome = true;
      packages = [
        pkgs.monero
        pkgs.monero-gui
      ];
      isNormalUser = true;
    };
  };
  security.sudo.extraConfig = ''
    ${mainUser.name} ALL=(bch) ALL
    ${mainUser.name} ALL=(bitcoin) ALL
    ${mainUser.name} ALL=(monero) ALL
  '';
}
