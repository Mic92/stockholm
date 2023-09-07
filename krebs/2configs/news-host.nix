{ config, ... }:
{
  krebs.sync-containers3.containers.news = {
    sshKey = "${config.krebs.secret.directory}/news.sync.key";
  };
}
