{ config, lib, pkgs, ... }:

with config.krebs.lib;
{
  krebs.enable = true;
  krebs.tinc.retiolum.enable = true;

  # TODO rename shared user to "krebs"
  krebs.build.user = mkDefault config.krebs.users.shared;
  krebs.build.source = let inherit (config.krebs.build) host user; in {
    nixos-config.symlink = "stockholm/${user.name}/1systems/${host.name}.nix";
    nixpkgs.git = {
      url = https://github.com/NixOS/nixpkgs;
      ref = "63b9785"; # stable @ 2016-06-01
    };
    secrets.file = "${getEnv "HOME"}/secrets/krebs/${host.name}";
    stockholm.file = "${getEnv "HOME"}/stockholm";
  };

  networking.hostName = config.krebs.build.host.name;

  nix.maxJobs = 1;
  nix.trustedBinaryCaches = [
    "https://cache.nixos.org"
    "http://cache.nixos.org"
    "http://hydra.nixos.org"
  ];
  nix.useChroot = true;

  nixpkgs.config.packageOverrides = pkgs: {
    nano = pkgs.vim;
  };

  environment.systemPackages = with pkgs; [
    git
    rxvt_unicode.terminfo
  ];

  programs.ssh.startAgent = false;

  services.openssh = {
    enable = true;
    hostKeys = [
      { type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
    ];
  };
  services.cron.enable = false;
  services.nscd.enable = false;
  services.ntp.enable = false;

  users.mutableUsers = false;
  users.extraUsers.root.openssh.authorizedKeys.keys = [
    # TODO
    config.krebs.users.lass.pubkey
    config.krebs.users.makefu.pubkey
    # TODO HARDER:
    config.krebs.users.makefu-omo.pubkey
    config.krebs.users.tv.pubkey
  ];


  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";

}
