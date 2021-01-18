{ config, pkgs, ... }:
{
  imports = [
    <stockholm/lass/2configs/container-networking.nix>
    <stockholm/lass/2configs/syncthing.nix>
  ];
  lass.sync-containers.containers.green = {
    peers = [
      "icarus"
      "shodan"
      "skynet"
      "mors"
      "littleT"
    ];
    hostIp = "10.233.2.15";
    localIp = "10.233.2.16";
    format = "ecryptfs";
  };
}
