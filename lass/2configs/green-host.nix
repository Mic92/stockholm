{ config, lib, pkgs, ... }:
with import <stockholm/lib>;

{
  imports = [
    <stockholm/lass/2configs/container-networking.nix>
    <stockholm/lass/2configs/syncthing.nix>
    { #hack for already defined
      systemd.services."container@green".reloadIfChanged = mkForce false;
      systemd.services."container@green".preStart = ''
        ${pkgs.mount}/bin/mount | ${pkgs.gnugrep}/bin/grep -q ' on /var/lib/containers/green '
      '';
      systemd.services."container@green".postStop = ''
        set -x
        ${pkgs.umount}/bin/umount /var/lib/containers/green
        ls -la /dev/mapper/control
        ${pkgs.devicemapper}/bin/dmsetup ls
        ${pkgs.cryptsetup}/bin/cryptsetup -v luksClose /var/lib/sync-containers/green.img
      '';
    }
  ];

  lass.ensure-permissions = [
    { folder = "/var/lib/sync-containers"; owner = "root"; group = "syncthing"; }
  ];

  krebs.syncthing.folders = [
    { path = "/var/lib/sync-containers"; peers = [ "icarus" "skynet" "littleT" "shodan" ]; }
  ];

  system.activationScripts.containerPermissions = ''
    mkdir -p /var/lib/containers
    chmod 711 /var/lib/containers
  '';

  containers.green = {
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
    hostAddress = "10.233.2.15";
    localAddress = "10.233.2.16";
  };

  environment.systemPackages = [
    (pkgs.writeDashBin "start-green" ''
      set -fu
      CONTAINER='green'
      IMAGE='/var/lib/sync-containers/green.img'

      ${pkgs.cryptsetup}/bin/cryptsetup status "$CONTAINER" >/dev/null
      if [ "$?" -ne 0 ]; then
        ${pkgs.cryptsetup}/bin/cryptsetup luksOpen "$IMAGE" "$CONTAINER"
      fi

      mkdir -p /var/lib/containers/"$CONTAINER"

      ${pkgs.mount}/bin/mount | grep -q " on /var/lib/containers/"$CONTAINER" "
      if [ "$?" -ne 0 ]; then
        ${pkgs.mount}/bin/mount -o sync /dev/mapper/"$CONTAINER" /var/lib/containers/"$CONTAINER"
      fi

      STATE=$(${pkgs.nixos-container}/bin/nixos-container status "$CONTAINER")
      if [ "$STATE" = 'down' ]; then
        ${pkgs.nixos-container}/bin/nixos-container start "$CONTAINER"
      fi
      ping -c1 green.r
      if [ "$?" -ne 0 ]; then
        ${pkgs.nixos-container}/bin/nixos-container run green -- nixos-rebuild -I /var/src switch
      fi

    '')
  ];
}
