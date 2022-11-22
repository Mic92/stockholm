{ config, pkgs, ... }:
{
  imports = [
    <stockholm/lass/2configs/container-networking.nix>
  ];

  lass.sync-containers3.containers.green = {
    sshKey = "${toString <secrets>}/green.sync.key";
  };
}
