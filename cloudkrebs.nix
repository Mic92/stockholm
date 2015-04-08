{ config, pkgs, ... }:

{
  imports = [
    ./modules/tv/base-cac-CentOS-7-64bit.nix
    ./modules/lass/retiolum-cloudkrebs.nix
    ./modules/lass/networking-cloudkrebs.nix
  ];

  nix.maxJobs = 1;

  #activationScripts
  #split up and move into base

  #TODO move into modules
  users.extraUsers = {
    #main user
    root = {
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAp83zynhIueJJsWlSEykVSBrrgBFKq38+vT8bRfa+csqyjZBl2SQFuCPo+Qbh49mwchpZRshBa9jQEIGqmXxv/PYdfBFQuOFgyUq9ZcTZUXqeynicg/SyOYFW86iiqYralIAkuGPfQ4howLPVyjTZtWeEeeEttom6p6LMY5Aumjz2em0FG0n9rRFY2fBzrdYAgk9C0N6ojCs/Gzknk9SGntA96MDqHJ1HXWFMfmwOLCnxtE5TY30MqSmkrJb7Fsejwjoqoe9Y/mCaR0LpG2cStC1+37GbHJNH0caCMaQCX8qdfgMVbWTVeFWtV6aWOaRgwLrPDYn4cHWQJqTfhtPrNQ== lass@mors"
      ];
      hashedPassword = "$6$3CFU7MPj$qr1cn6p2kvZRdt1pueBLC1WLzV7KPbk8Qi11Wq8l2sVT0JFMf8BOULJOx8xS1KXqZHnUW1p7VN3dZ1VUepQGj.";
    };
    lass = {
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
        "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAp83zynhIueJJsWlSEykVSBrrgBFKq38+vT8bRfa+csqyjZBl2SQFuCPo+Qbh49mwchpZRshBa9jQEIGqmXxv/PYdfBFQuOFgyUq9ZcTZUXqeynicg/SyOYFW86iiqYralIAkuGPfQ4howLPVyjTZtWeEeeEttom6p6LMY5Aumjz2em0FG0n9rRFY2fBzrdYAgk9C0N6ojCs/Gzknk9SGntA96MDqHJ1HXWFMfmwOLCnxtE5TY30MqSmkrJb7Fsejwjoqoe9Y/mCaR0LpG2cStC1+37GbHJNH0caCMaQCX8qdfgMVbWTVeFWtV6aWOaRgwLrPDYn4cHWQJqTfhtPrNQ== lass@mors"
      ];
      hashedPassword = "$6$3CFU7MPj$qr1cn6p2kvZRdt1pueBLC1WLzV7KPbk8Qi11Wq8l2sVT0JFMf8BOULJOx8xS1KXqZHnUW1p7VN3dZ1VUepQGj.";
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
