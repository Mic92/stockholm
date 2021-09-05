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
}
