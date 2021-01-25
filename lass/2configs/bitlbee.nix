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
      # pkgs.telegram-purple
      pkgs.tdlib-purple
      # pkgs.purple-gowhatsapp
    ];
  };
}
