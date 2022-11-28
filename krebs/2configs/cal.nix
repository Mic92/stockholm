{ config, lib, pkgs, ... }:
{
  users.users.testing = {
    uid = pkgs.stockholm.lib.genid_uint31 "testing";
    isNormalUser = true;
    openssh.authorizedKeys.keys = [
      config.krebs.users.xkey.pubkey
      config.krebs.users.lass.pubkey
    ];
    packages = [
      pkgs.calendar-cli
      pkgs.tmux
    ];
  };

  services.xandikos = {
    enable = true;
    extraOptions = [
      "--autocreate"
      "--defaults"
      "--current-user-principal /krebs"
      "--dump-dav-xml"
    ];
  };

  services.nginx = {
    enable = true;

    virtualHosts = {
      "calendar.r".locations."/".proxyPass = "http://localhost:${toString config.services.xandikos.port}/";
    };
  };
}
