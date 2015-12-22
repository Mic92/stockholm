{ config, lib, pkgs, ... }:

with lib;
{
  krebs.enable = true;
  krebs.retiolum = {
    enable = true;
    connectTo = [
      # TODO remove connectTo cd, this was only used for bootstrapping
      "cd"
      "gum"
      "pigstarter"
    ];
  };

  krebs.build.source = {
    git.nixpkgs = {
      url = https://github.com/NixOS/nixpkgs;
      rev = "6d31e9b81dcd4ab927bb3dc91b612dd5abfa2f80";
    };
    dir.secrets = {
      host = config.krebs.current.host;
      path = mkDefault "${getEnv "HOME"}/secrets/krebs/wolf";
    };
    dir.stockholm = {
      host = config.krebs.current.host;
      path = mkDefault "${getEnv "HOME"}/stockholm";
    };
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
    (readFile ../../krebs/Zpubkeys/makefu_omo.ssh.pub)
    config.krebs.users.tv.pubkey
  ];


  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";

}
