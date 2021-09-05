{ config, pkgs, ... }:
let
  light-shack-src = 
    pkgs.fetchFromGitHub {
      owner = "shackspace";
      repo = "standby.shack";
      rev = "e1b90a0a";
      sha256 = "07fmz63arc5rxa0a3778srwz0jflp4ad6xnwkkc56hwybby0bclh";
    };
  web-dir = "${light-shack-src}/client/www/";
in
{
  services.nginx.virtualHosts."light.shack".locations."/".root = web-dir;
}
