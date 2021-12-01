with (import <stockholm/lib>);
{ config, lib, pkgs, ... }:

{
  services.bitlbee = {
    enable = true;
    portNumber = 6666;
    plugins = [
      pkgs.bitlbee-facebook
      pkgs.bitlbee-steam
      pkgs.bitlbee-discord
    ];
    libpurple_plugins = [
      pkgs.telegram-purple
      # pkgs.tdlib-purple
      # pkgs.purple-gowhatsapp
    ];
  };

  users.users.bitlbee = {
    uid = genid_uint31 "bitlbee";
    isSystemUser = true;
    group = "bitlbee";
  };
  users.groups.bitlbee = {};

  systemd.services.bitlbee.serviceConfig = {
    DynamicUser = lib.mkForce false;
    User = "bitlbee";
    StateDirectory = lib.mkForce null;
  };
}
