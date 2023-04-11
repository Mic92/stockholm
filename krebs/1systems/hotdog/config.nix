{ config, lib, pkgs, ... }:

{
  imports = [
    <stockholm/krebs>
    <stockholm/krebs/2configs>

    <stockholm/krebs/2configs/buildbot-stockholm.nix>
    <stockholm/krebs/2configs/binary-cache/nixos.nix>
    <stockholm/krebs/2configs/ircd.nix>
    <stockholm/krebs/2configs/reaktor2.nix>
    <stockholm/krebs/2configs/wiki.nix>
    <stockholm/krebs/2configs/acme.nix>
    <stockholm/krebs/2configs/mud.nix>
    <stockholm/krebs/2configs/repo-sync.nix>

    <stockholm/krebs/2configs/cal.nix>
    <stockholm/krebs/2configs/mastodon.nix>

    ## shackie irc bot
    <stockholm/krebs/2configs/shack/reaktor.nix>
  ];

  krebs.build.host = config.krebs.hosts.hotdog;
  krebs.pages.enable = true;

  boot.isContainer = true;
  networking.useDHCP = false;
  krebs.sync-containers3.inContainer = {
    enable = true;
    pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM20tYHHvwIgrJZzR35ATzH9AlTrM1enNKEQJ7IP6lBh";
  };
}
