{ config, pkgs, ... }:

{
  imports = [
    ../.
    ../2configs/hw/tp-x220.nix
    ../2configs/baseX.nix
    ../2configs/exim-retiolum.nix
    ../2configs/programs.nix
    ../2configs/bitcoin.nix
    ../2configs/browsers.nix
    ../2configs/games.nix
    ../2configs/pass.nix
    ../2configs/virtualbox.nix
    ../2configs/elster.nix
    ../2configs/steam.nix
    ../2configs/wine.nix
    ../2configs/chromium-patched.nix
    ../2configs/git.nix
    ../2configs/bitlbee.nix
    ../2configs/skype.nix
    ../2configs/teamviewer.nix
    ../2configs/libvirt.nix
    ../2configs/fetchWallpaper.nix
    ../2configs/c-base.nix
    ../2configs/mail.nix
    ../2configs/krebs-pass.nix
    ../2configs/umts.nix
    {
      #risk of rain port
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp --dport 11100"; target = "ACCEPT"; }
      ];
    }
    #{
    #  services.mysql = {
    #    enable = true;
    #    package = pkgs.mariadb;
    #    rootPassword = "<secrets>/mysql_rootPassword";
    #  };
    #}
    #{
    #  services.elasticsearch = {
    #    enable = true;
    #    plugins = [
    #    #  pkgs.elasticsearchPlugins.elasticsearch_kopf
    #    ];
    #  };
    #}
    #{
    #  services.postgresql = {
    #    enable = true;
    #    package = pkgs.postgresql;
    #  };
    #}
  ];

  krebs.build.host = config.krebs.hosts.mors;

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
      device = "/dev/big/nix";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/sda1";
    };

    "/mnt/loot" = {
      device = "/dev/big/loot";
      fsType = "ext4";
    };

    "/home" = {
      device = "/dev/big/home";
      fsType = "ext4";
    };

    "/home/lass" = {
      device = "/dev/big/home-lass";
      fsType = "ext4";
    };

    "/bku" = {
      device = "/dev/big/backups";
      fsType = "ext4";
    };

    "/home/games/.local/share/Steam" = {
      device = "/dev/big/steam";
      fsType = "ext4";
    };

    "/home/virtual/virtual" = {
      device = "/dev/big/virtual";
      fsType = "ext4";
    };

    "/mnt/public" = {
      device = "/dev/big/public";
      fsType = "ext4";
    };

    "/mnt/conf" = {
      device = "/dev/big/conf";
      fsType = "ext4";
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="00:24:d7:f0:a0:0c", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:8f:85:c9", NAME="et0"
  '';

  #TODO activationScripts seem broken, fix them!
  #activationScripts
  #split up and move into base
  system.activationScripts.powertopTunables = ''
    #Enable Audio codec power management
    echo '1' > '/sys/module/snd_hda_intel/parameters/power_save'
    #VM writeback timeout
    echo '1500' > '/proc/sys/vm/dirty_writeback_centisecs'
    #Autosuspend for USB device Broadcom Bluetooth Device [Broadcom Corp]
    #echo 'auto' > '/sys/bus/usb/devices/1-1.4/power/control'
    #Autosuspend for USB device Biometric Coprocessor
    #echo 'auto' > '/sys/bus/usb/devices/1-1.3/power/control'

    #Runtime PMs
    echo 'auto' > '/sys/bus/pci/devices/0000:00:02.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:16.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:00.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:03:00.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1f.3/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1f.2/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1f.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1d.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1c.3/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:0d:00.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1c.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1b.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1a.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:19.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:16.3/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1c.1/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1c.4/power/control'
  '';

  environment.systemPackages = with pkgs; [
    acronym
    cac-api
    sshpass
    get
    teamspeak_client
    hashPassword
    urban
    mk_sql_pair
    remmina
  ];

  #TODO: fix this shit
  ##fprint stuff
  ##sudo fprintd-enroll $USER to save fingerprints
  #services.fprintd.enable = true;
  #security.pam.services.sudo.fprintAuth = true;

  users.extraGroups = {
    loot = {
      members = [
        config.users.extraUsers.mainUser.name
        "firefox"
        "chromium"
        "google"
        "virtual"
      ];
    };
  };

  services.mongodb = {
    enable = true;
  };
}
