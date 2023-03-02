{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>

    <stockholm/krebs/2configs/ircd.nix>
    <stockholm/krebs/2configs/go.nix>

    #### NEWS ####
    <stockholm/krebs/2configs/ircd.nix>
    <stockholm/krebs/2configs/news.nix>
  ];

  krebs.build.host = config.krebs.hosts.news;

  boot.isContainer = true;
  networking.useDHCP = lib.mkForce true;
  krebs.sync-containers3.inContainer = {
    enable = true;
    pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMBVZomw68WDQy0HsHhNbWK1KpzaR5aRUG1oioE7IgCv";
  };
}
