{ pkgs, ... }:

{
  imports = [ ./consul-server.nix ];

  tv.consul = {
    server = pkgs.lib.mkForce false;
  };
}
