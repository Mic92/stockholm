{ config, lib, pkgs, ... }:
with import <stockholm/lib>;

{
  imports = [
    <stockholm/lass/2configs/container-networking.nix>
  ];
  containers.blue = {
    config = { ... }: {
      environment.systemPackages = [ pkgs.git ];
      services.openssh.enable = true;
      users.users.root.openssh.authorizedKeys.keys = [
        config.krebs.users.lass.pubkey
      ];
    };
    autoStart = true;
    enableTun = true;
    privateNetwork = true;
    hostAddress = "10.233.2.9";
    localAddress = "10.233.2.10";
  };
}
