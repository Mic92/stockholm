{ config, lib, pkgs, ... }:

let
  inherit (lib) head;

  ip = (head config.krebs.build.host.nets.internet.addrs4);
in {
  imports = [
    ../2configs/base.nix
    ../2configs/downloading.nix
    ../2configs/git.nix
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

    }
    {
      sound.enable = false;
    }
    {
      #workaround for server dying after 6-7h
      boot.kernelPackages = pkgs.linuxPackages_4_2;
    }
  ];

  krebs.build.host = config.krebs.hosts.prism;
}
