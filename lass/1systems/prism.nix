{ config, lib, pkgs, ... }:

let
  ip = config.krebs.build.host.nets.internet.ip4.addr;
in {
  imports = [
    ../.
    ../2configs/base.nix
    ../2configs/exim-smarthost.nix
    ../2configs/downloading.nix
    ../2configs/git.nix
    ../2configs/ts3.nix
    ../2configs/bitlbee.nix
    ../2configs/weechat.nix
    {
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
    }
    {
      networking.interfaces.et0.ip4 = [
        {
          address = ip;
          prefixLength = 24;
        }
      ];
      networking.defaultGateway = "213.239.205.225";
      networking.nameservers = [
        "8.8.8.8"
      ];
      services.udev.extraRules = ''
        SUBSYSTEM=="net", ATTR{address}=="54:04:a6:7e:f4:06", NAME="et0"
      '';

    }
    {
      #boot.loader.gummiboot.enable = true;
      #boot.loader.efi.canTouchEfiVariables = true;
      boot.loader.grub = {
        devices = [
          "/dev/sda"
          "/dev/sdb"
        ];
        splashImage = null;
      };

      boot.initrd.availableKernelModules = [
        "ata_piix"
        "vmw_pvscsi"
      ];

      fileSystems."/" = {
        device = "/dev/pool/nix";
        fsType = "ext4";
      };

      fileSystems."/boot" = {
        device = "/dev/disk/by-uuid/7ca12d8c-606d-41ce-b10d-62b654e50e36";
      };

      fileSystems."/var/download" = {
        device = "/dev/pool/download";
      };

      fileSystems."/srv/http" = {
        device = "/dev/pool/http";
      };

      fileSystems."/srv/o.ubikmedia.de-data" = {
        device = "/dev/pool/owncloud-ubik-data";
      };

      fileSystems."/bku" = {
        device = "/dev/pool/bku";
      };

    }
    {
      sound.enable = false;
    }
    #{
    #  #workaround for server dying after 6-7h
    #  boot.kernelPackages = pkgs.linuxPackages_4_2;
    #}
    {
      nixpkgs.config.allowUnfree = true;
    }
    {
      #stuff for juhulian
      users.extraUsers.juhulian = {
        name = "juhulian";
        uid = 1339;
        home = "/home/juhulian";
        group = "users";
        createHome = true;
        useDefaultShell = true;
        extraGroups = [
        ];
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDBQhLGvfv4hyQ/nqJGy1YgHXPSVl6igeWTroJSvAhUFgoh+rG+zvqY0EahKXNb3sq0/OYDCTJVuucc0hgCg7T2KqTqMtTb9EEkRmCFbD7F7DWZojCrh/an6sHneqT5eFvzAPZ8E5hup7oVQnj5P5M3I9keRHBWt1rq6q0IcOEhsFvne4qJc73aLASTJkxzlo5U8ju3JQOl6474ECuSn0lb1fTrQ/SR1NgF7jV11eBldkS8SHEB+2GXjn4Yrn+QUKOnDp+B85vZmVlJSI+7XR1/U/xIbtAjGTEmNwB6cTbBv9NCG9jloDDOZG4ZvzzHYrlBXjaigtQh2/4mrHoKa5eV juhulian@juhulian"
        ];
      };
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p udp --dport 60000:61000"; target = "ACCEPT";}
      ];
    }
    {
      environment.systemPackages = [
        pkgs.perlPackages.Plack
      ];
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp --dport 8080"; target = "ACCEPT";}
      ];
    }
    {
      users.users.chat.openssh.authorizedKeys.keys = [
        "ecdsa-sha2-nistp384 AAAAE2VjZHNhLXNoYTItbmlzdHAzODQAAAAIbmlzdHAzODQAAABhBBQjn/3n283RZkBs2CFqbpukyQ3zkLIjewRpKttPa5d4PUiT7/vOlutWH5EP4BxXQSoeZStx8D2alGjxfK+nfDvRJGGofpm23cN4j4i24Fcam1y1H7wqRXO1qbz5AB3qPg== JuiceSSH"
        config.krebs.users.lass-uriel.pubkey
      ];
    }
    {
      time.timeZone = "Europe/Berlin";
    }
    {
      imports = [
        ../2configs/websites/wohnprojekt-rhh.de.nix
        ../2configs/websites/domsen.nix
      ];
      krebs.iptables.tables.filter.INPUT.rules = [
         { predicate = "-p tcp --dport http"; target = "ACCEPT"; }
         { predicate = "-p tcp --dport https"; target = "ACCEPT"; }
      ];
    }
    {
      services.tor = {
        enable = true;
        client.enable = true;
      };
    }
  ];

  krebs.build.host = config.krebs.hosts.prism;
}
