{ config, pkgs, ... }:

{
  imports =
    [
      <secrets/hashedPasswords.nix>
      ./modules/base-cac-CentOS-7-64bit.nix
      ./modules/exim-cd.nix
      ./modules/ejabberd-cd.nix # XXX echtes modul
      ./modules/iptables-cd.nix
      ./modules/networking-cd.nix
      ./modules/retiolum.nix
    ];

  # "Developer 2" plan has two vCPUs.
  nix.maxJobs = 2;


  environment.systemPackages = with pkgs; [
    htop
    iftop
    iotop
    mutt    # for mv
    nethogs
    rxvt_unicode.terminfo
    tcpdump
  ];

  security.rtkit.enable = false;

  services.cron.enable = false;

  services.ejabberd-cd = {
    enable = true;
  };

  services.journald.extraConfig = ''
    SystemMaxUse=1G
    RuntimeMaxUse=128M
  '';

  services.ntp.enable = false;

  services.openssh = {
    enable = true;
    hostKeys = [
      # XXX bits here make no science
      { bits = 8192; type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
    ];
    permitRootLogin = "yes";
  };

  services.retiolum = {
    enable = true;
    hosts = ./hosts;
    privateKeyFile = "/etc/nixos/secrets/cd.retiolum.rsa_key.priv";
    connectTo = [
      "fastpoke"
      "pigstarter"
      "ire"
    ];
  };

  sound.enable = false;

  # TODO base
  time.timeZone = "UTC";

  # TODO replace by ./modules/cd-users.nix
  users.extraGroups = {

    # ● systemd-tmpfiles-setup.service - Create Volatile Files and Directories
    #    Loaded: loaded (/nix/store/2l33gg7nmncqkpysq9f5fxyhlw6ncm2j-systemd-217/example/systemd/system/systemd-tmpfiles-setup.service)
    #    Active: failed (Result: exit-code) since Mon 2015-03-16 10:29:18 UTC; 4s ago
    #      Docs: man:tmpfiles.d(5)
    #            man:systemd-tmpfiles(8)
    #   Process: 19272 ExecStart=/nix/store/2l33gg7nmncqkpysq9f5fxyhlw6ncm2j-systemd-217/bin/systemd-tmpfiles --create --remove --boot --exclude-prefix=/dev (code=exited, status=1/FAILURE)
    #  Main PID: 19272 (code=exited, status=1/FAILURE)
    # 
    # Mar 16 10:29:17 cd systemd-tmpfiles[19272]: [/usr/lib/tmpfiles.d/legacy.conf:26] Unknown group 'lock'.
    # Mar 16 10:29:18 cd systemd-tmpfiles[19272]: Two or more conflicting lines for /var/log/journal configured, ignoring.
    # Mar 16 10:29:18 cd systemd-tmpfiles[19272]: Two or more conflicting lines for /var/log/journal/7b35116927d74ea58785e00b47ac0f0d configured, ignoring.
    # Mar 16 10:29:18 cd systemd[1]: systemd-tmpfiles-setup.service: main process exited, code=exited, status=1/FAILURE
    # Mar 16 10:29:18 cd systemd[1]: Failed to start Create Volatile Files and Directories.
    # Mar 16 10:29:18 cd systemd[1]: Unit systemd-tmpfiles-setup.service entered failed state.
    # Mar 16 10:29:18 cd systemd[1]: systemd-tmpfiles-setup.service failed.
    # warning: error(s) occured while switching to the new configuration
    lock.gid = 10001;

  };
  users.extraUsers =
    {
      root = {
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEieAihh+o208aeCA14fAtjzyZN/nrpOJt2vZ5VYZp69 deploy@wu"
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDDYv5OkVrnerkzJwgi7ol7HzcWJf4oWCJTX84trFX5vgJXu1zMvSe+koY8xpnMOd7WHF2wgsjjrFlMuixTrfMPc/OjvG2N1TlnvzlFD8ivTW/AJzDwNxT//niqAYAZ9kmb8e/zE/SyNHSKZcyEKGiiW2+YW9wWHPYRP/XiNEjLP3BeTGScMwWr001V/8m7ne4SGHrE1FbHbHqaBXgqUFgnvzMY3CsfDafODZlj5xSMNGHyLGNNKvu3YR1crcAjbQrBXBdwaArThFxp+e2uWrnffshlks6WtRyR1AFVjc/gxEG74Axq1AHY6EJm2Fw/JdFNiYQ7yyQZHS9bZJYjgnWF tv@nomic"
        ];
      };

      mv = rec {
        name = "mv";
        uid = 1338;
        group = "users";
        home = "/home/${name}";
        createHome = true;
        useDefaultShell = true;
        openssh.authorizedKeys.keys = [
          "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGer9e2+Lew7vnisgBbsFNECEIkpNJgEaqQqgb9inWkQ mv@vod"
        ];
      };

    };

  users.mutableUsers = false;

}
