{ config, pkgs, ... }:

{
  imports = [
    ../tv/base-cac-CentOS-7-64bit.nix
    ../lass/retiolum-cloudkrebs.nix
    ./networking.nix
    ../../secrets/cloudkrebs-pw.nix
    ../lass/sshkeys.nix
    ../common/nixpkgs.nix
  ];

  nixpkgs = {
    url = "https://github.com/Lassulus/nixpkgs";
    rev = "b42ecfb8c61e514bf7733b4ab0982d3e7e27dacb";
  };

  nix.maxJobs = 1;

  #activationScripts
  #split up and move into base

  #TODO move into modules
  users.extraUsers = {
    #main user
    root = {
      openssh.authorizedKeys.keys = [
        config.sshKeys.lass.pub
      ];
    };
    mainUser = {
      uid = 1337;
      name = "lass";
      #isNormalUser = true;
      group = "users";
      createHome = true;
      home = "/home/lass";
      useDefaultShell = true;
      isSystemUser = false;
      description = "lassulus";
      extraGroups = [ "wheel" ];
      openssh.authorizedKeys.keys = [
        config.sshKeys.lass.pub
      ];
    };
  };

  environment.systemPackages = with pkgs; [
  ];

  services.openssh = {
    enable = true;
    hostKeys = [
      # XXX bits here make no science
      { bits = 8192; type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
    ];
    permitRootLogin = "yes";
  };

  networking.firewall = {
    enable = true;

    allowedTCPPorts = [
      22
    ];
  };

}
