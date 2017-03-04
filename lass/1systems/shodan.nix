{ config, pkgs, ... }:

with import <stockholm/lib>;
{
  imports = [
    ../.
    ../2configs/retiolum.nix
    ../2configs/hw/tp-x220.nix
    ../2configs/baseX.nix
    ../2configs/git.nix
    ../2configs/exim-retiolum.nix
    ../2configs/browsers.nix
    ../2configs/programs.nix
    ../2configs/fetchWallpaper.nix
    ../2configs/backups.nix
    #{
    #  users.extraUsers = {
    #    root = {
    #      openssh.authorizedKeys.keys = map readFile [
    #        ../../krebs/Zpubkeys/uriel.ssh.pub
    #      ];
    #    };
    #  };
    #}
    {
      users.users.sokratess = {
        uid = genid "sokratess";
        home = "/home/sokratess";
        group = "users";
        createHome = true;
        extraGroups = [
         "audio"
          "networkmanager"
        ];
        useDefaultShell = true;
        password = "aidsballs";
      };
      krebs.per-user.sokratess.packages = [
        pkgs.firefox
        pkgs.python27Packages.virtualenv
        pkgs.python27Packages.ipython
        pkgs.python27Packages.python
      ];
    }
    {
      krebs.monit = let
        echoToIrc = msg:
          pkgs.writeDash "echo_irc" ''
            set -euf
            export LOGNAME=prism-alarm
            ${pkgs.irc-announce}/bin/irc-announce \
              ni.r 6667 ${config.networking.hostName}-alarm \#noise "${msg}" >/dev/null
          '';
      in {
        enable = true;
        http.enable = true;
        alarms = {
          hfos = {
            test = "${pkgs.curl}/bin/curl -sf --insecure 'https://hfos.hackerfleet.de'";
            alarm = echoToIrc "test hfos failed";
          };
        };
      };
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp -i retiolum --dport 9093"; target = "ACCEPT"; }
      ];
    }
  ];

  krebs.build.host = config.krebs.hosts.shodan;

  boot = {
    loader.grub.enable = true;
    loader.grub.version = 2;
    loader.grub.device = "/dev/sda";

    initrd.luks.devices = [ { name = "luksroot"; device = "/dev/sda2"; } ];
    initrd.luks.cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
    initrd.availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" ];
    #kernelModules = [ "kvm-intel" "msr" ];
  };
  fileSystems = {
    "/" = {
      device = "/dev/pool/nix";
      fsType = "btrfs";
    };

    "/boot" = {
      device = "/dev/sda1";
    };

    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = ["nosuid" "nodev" "noatime"];
    };
    "/bku" = {
      device = "/dev/pool/bku";
      fsType = "btrfs";
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="a0:88:b4:29:26:bc", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:0c:a7:63", NAME="et0"
  '';
}
