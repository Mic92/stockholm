{ config, lib, pkgs, ... }:

{
  imports = [
    ../../../krebs
    ../../../krebs/2configs

    ../../../krebs/2configs/ircd.nix
    ../../../krebs/2configs/go.nix

    #### NEWS ####
    ../../../krebs/2configs/ircd.nix
    ../../../krebs/2configs/news.nix
  ];

  krebs.build.host = config.krebs.hosts.news;

  boot.isContainer = true;
  networking.useDHCP = lib.mkForce true;
  krebs.sync-containers3.inContainer = {
    enable = true;
    pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMBVZomw68WDQy0HsHhNbWK1KpzaR5aRUG1oioE7IgCv";
  };
}
