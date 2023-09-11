{ config, ... }:
{
  krebs.sync-containers3.containers.hotdog = {
    sshKey = "${config.krebs.secret.directory}/hotdog.sync.key";
  };
  containers.hotdog.bindMounts."/var/lib" = {
    hostPath = "/var/lib/sync-containers3/hotdog/state";
    isReadOnly = false;
  };
}
