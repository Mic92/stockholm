{ config,lib, ... }:
{
  nixpkgs.config.allowUnfree = true; #  "consul-1.18.0" 
  krebs.sync-containers3.containers.news = {
    sshKey = "${config.krebs.secret.directory}/news.sync.key";
  };
}
