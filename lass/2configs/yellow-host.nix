{ config, pkgs, ... }:
{
  krebs.sync-containers3.containers.yellow = {
    sshKey = "${toString <secrets>}/yellow.sync.key";
  };
  containers.yellow.bindMounts."/var/lib" = {
    hostPath = "/var/lib/sync-containers3/yellow/state";
    isReadOnly = false;
  };
  containers.yellow.bindMounts."/var/download" = {
    hostPath = "/var/download";
    isReadOnly = false;
  };
}
