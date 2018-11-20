{ config, lib, pkgs, ... }:
with import <stockholm/lib>;

{
  imports = [
    <stockholm/lass/2configs/container-networking.nix>
  ];
  systemd.services."container@blue".reloadIfChanged = mkForce false;
  containers.blue = {
    config = { ... }: {
      environment.systemPackages = [
        pkgs.git
        pkgs.rxvt_unicode.terminfo
      ];
      services.openssh.enable = true;
      users.users.root.openssh.authorizedKeys.keys = [
        config.krebs.users.lass.pubkey
      ];
    };
    autoStart = false;
    enableTun = true;
    privateNetwork = true;
    hostAddress = "10.233.2.9";
    localAddress = "10.233.2.10";
  };
  environment.systemPackages = [
    (pkgs.writeDashBin "start-blue" ''
      set -ef
      if ping -c1 blue.r; then
        echo 'blue is already running. bailing out'
        exit 23
      fi
      if ! $(mount | ${pkgs.gnugrep}/bin/grep -qi '^encfs on /var/lib/containers/blue'); then
        ${pkgs.encfs}/bin/encfs --public /var/lib/containers/.blue /var/lib/containers/blue
      fi
      nixos-container start blue
      nixos-container run blue -- nixos-rebuild -I /var/src switch
    '')
    (pkgs.writeDashBin "stop-blue" ''
      set -ef
      nixos-container stop blue
      fusermount -u /var/lib/containers/blue
    '')
  ];
}
