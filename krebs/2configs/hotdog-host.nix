{
  krebs.sync-containers3.containers.hotdog = {
    sshKey = "${toString <secrets>}/hotdog.sync.key";
  };
  containers.hotdog.bindMounts."/var/lib" = {
    hostPath = "/var/lib/sync-containers3/hotdog/state";
    isReadOnly = false;
  };
}
