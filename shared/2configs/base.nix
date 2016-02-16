{ config, lib, pkgs, ... }:

with config.krebs.lib;
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

  # TODO rename shared user to "krebs"
  krebs.build.user = mkDefault config.krebs.users.shared;
  krebs.build.source = {
    nixpkgs = mkDefault {
      url = https://github.com/NixOS/nixpkgs;
      rev = "77f8f35d57618c1ba456d968524f2fb2c3448295"; # for urlwatch-minidb
    };
    secrets =  mkDefault "${getEnv "HOME"}/secrets/krebs/${config.krebs.build.host.name}";
    stockholm = mkDefault "${getEnv "HOME"}/stockholm";

    nixos-config = "symlink:stockholm/${config.krebs.build.user.name}/1systems/${config.krebs.build.host.name}.nix";
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
