{ config, lib, pkgs, ... }:

{
  imports = [
    ../../../krebs
    ../../../krebs/2configs

    ../../../krebs/2configs/buildbot-stockholm.nix
    ../../../krebs/2configs/binary-cache/nixos.nix
    ../../../krebs/2configs/ircd.nix
    ../../../krebs/2configs/reaktor2.nix
    ../../../krebs/2configs/wiki.nix
    ../../../krebs/2configs/acme.nix
    ../../../krebs/2configs/mud.nix
    ../../../krebs/2configs/repo-sync.nix

    ../../../krebs/2configs/cal.nix
    ../../../krebs/2configs/mastodon.nix

    ## (shackie irc bot
    ../../../krebs/2configs/shack/reaktor.nix
  ];

  krebs.build.host = config.krebs.hosts.hotdog;
  krebs.hosts.hotdog.ssh.privkey.path = <secrets/ssh.id_ed25519>;
  krebs.pages.enable = true;

  boot.isContainer = true;
  networking.useDHCP = false;
  krebs.sync-containers3.inContainer = {
    enable = true;
    pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM20tYHHvwIgrJZzR35ATzH9AlTrM1enNKEQJ7IP6lBh";
  };
}
