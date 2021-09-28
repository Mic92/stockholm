{ config, lib, pkgs, ... }:
{
  krebs.reaktor2.shackie = {
    hostname = "irc.libera.chat";
    port = "6697";
    nick = "shackie";
    API.listen = "inet://127.0.0.1:7777";
    plugins = [
      {
        plugin = "register";
        config = {
          channels = [
            "#shackspace"
          ];
        };
      }
    ];
  };
  systemd.services.announce_doorstatus = {
    startAt = "*:0/1";
    path = with pkgs; [ curl jq ];
    script = builtins.readFile ./doorstatus.sh;
    serviceConfig = {
      DynamicUser = true;
      StateDirectory = "doorstatus";
      WorkingDirectory = "/var/lib/doorstatus";
      PrivateTmp = true;
    };
  };
}
