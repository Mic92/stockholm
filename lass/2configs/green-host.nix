{ config, pkgs, ... }:
{
  imports = [
    <stockholm/lass/2configs/container-networking.nix>
    <stockholm/lass/2configs/syncthing.nix>
  ];
  krebs.sync-containers.containers.green = {
    peers = [
      "icarus"
      "shodan"
      "skynet"
      "mors"
      "morpheus"
      "littleT"
      "styx"
    ];
    hostIp = "10.233.2.15";
    localIp = "10.233.2.16";
    format = "ecryptfs";
  };

  services.borgbackup.jobs.sync-green = {
    encryption.mode = "none";
    paths = "/var/lib/sync-containers/green/ecryptfs";
    repo = "/var/lib/sync-containers/green/backup";
    compression = "auto,lzma";
    startAt = "daily";
  };
}
