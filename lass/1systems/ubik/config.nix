with import <stockholm/lib>;
{ config, lib, pkgs, ... }:
{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs>
    <stockholm/lass/2configs/retiolum.nix>
  ];

  krebs.build.host = config.krebs.hosts.ubik;

  lass.sync-containers3.inContainer = {
    enable = true;
    pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIPBFGMjH0+Dco6DVFZbByENMci8CFTLXCL7j53yctPnM";
  };

  networking.firewall.allowedTCPPorts = [ 80 ];
  services.nextcloud = {
    enable = true;
    hostName = "c.apanowicz.de";
    package = pkgs.nextcloud25;
    config.adminpassFile = "/run/nextcloud.pw";
    https = true;
  };
  systemd.services.nextcloud-setup.serviceConfig.ExecStartPre = [
    "+${pkgs.writeDash "copy-pw" ''
      ${pkgs.rsync}/bin/rsync \
        --chown nextcloud:nextcloud \
        --chmod 0700 \
        /var/src/secrets/nextcloud.pw /run/nextcloud.pw
    ''}"
  ];
}
