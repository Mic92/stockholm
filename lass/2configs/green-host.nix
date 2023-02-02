{ config, pkgs, ... }:
{
  krebs.sync-containers3.containers.green = {
    sshKey = "${toString <secrets>}/green.sync.key";
  };
}
