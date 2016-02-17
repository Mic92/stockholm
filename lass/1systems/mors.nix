{ config, pkgs, ... }:

{
  imports = [
    ../.
    ../2configs/baseX.nix
    ../2configs/programs.nix
    ../2configs/bitcoin.nix
    ../2configs/browsers.nix
    ../2configs/games.nix
    ../2configs/pass.nix
    ../2configs/virtualbox.nix
    ../2configs/elster.nix
    ../2configs/steam.nix
    ../2configs/wine.nix
    ../2configs/texlive.nix
    ../2configs/binary-caches.nix
    #../2configs/ircd.nix
    ../2configs/chromium-patched.nix
    ../2configs/git.nix
    #../2configs/wordpress.nix
    ../2configs/bitlbee.nix
    #../2configs/firefoxPatched.nix
    ../2configs/skype.nix
    ../2configs/teamviewer.nix
    ../2configs/libvirt.nix
    ../2configs/fetchWallpaper.nix
    #../2configs/buildbot-standalone.nix
    {
      #risk of rain port
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp --dport 11100"; target = "ACCEPT"; }
      ];
    }
    {
      #static-nginx-test
      imports = [
        ../3modules/static_nginx.nix
      ];
      lass.staticPage."testserver.de" = {
        #sslEnable = true;
        #certificate = "${toString <secrets>}/testserver.de/server.cert";
        #certificate_key = "${toString <secrets>}/testserver.de/server.pem";
        ssl = {
          enable = true;
          certificate = "${toString <secrets>}/testserver.de/server.cert";
          certificate_key = "${toString <secrets>}/testserver.de/server.pem";
        };
      };
      networking.extraHosts = ''
        10.243.0.2 testserver.de
      '';
    }
    #{
    #  #wordpress-test
    #  #imports = singleton (sitesGenerators.createWordpress "testserver.de");
    #  imports = [
    #    ../3modules/wordpress_nginx.nix
    #  ];
    #  lass.wordpress."testserver.de" = {
    #    multiSite = {
    #      "1" = "testserver.de";
    #      "2" = "bla.testserver.de";
    #    };
    #  };

    #  services.mysql = {
    #    enable = true;
    #    package = pkgs.mariadb;
    #    rootPassword = "<secrets>/mysql_rootPassword";
    #  };
    #  networking.extraHosts = ''
    #    10.243.0.2 testserver.de
    #  '';
    #  krebs.iptables.tables.filter.INPUT.rules = [
    #    { predicate = "-i retiolum -p tcp --dport 80"; target = "ACCEPT"; precedence = 9998; }
    #  ];
    #}
    #{
    #  #owncloud-test
    #  #imports = singleton (sitesGenerators.createWordpress "testserver.de");
    #  imports = [
    #    ../3modules/owncloud_nginx.nix
    #  ];
    #  lass.owncloud."owncloud-test.de" = {
    #  };

    #  #services.mysql = {
    #  #  enable = true;
    #  #  package = pkgs.mariadb;
    #  #  rootPassword = "<secrets>/mysql_rootPassword";
    #  #};
    #  networking.extraHosts = ''
    #    10.243.0.2 owncloud-test.de
    #  '';
    #  krebs.iptables.tables.filter.INPUT.rules = [
    #    { predicate = "-i retiolum -p tcp --dport 80"; target = "ACCEPT"; precedence = 9998; }
    #  ];
    #}
    {
      containers.pythonenv = {
        config = {
          services.openssh.enable = true;
          users.users.root.openssh.authorizedKeys.keys = [
            config.krebs.users.lass.pubkey
          ];

          environment = {
            systemPackages = with pkgs; [
              git
              libxml2
              libxslt
              libzip
              python27Full
              python27Packages.buildout
              stdenv
              zlib
            ];

            pathsToLink = [ "/include" ];

            shellInit = ''
              # help pip to find libz.so when building lxml
              export LIBRARY_PATH=/var/run/current-system/sw/lib
              # ditto for header files, e.g. sqlite
              export C_INCLUDE_PATH=/var/run/current-system/sw/include
            '';
          };

        };
      };
    }
    {
      services.mysql = {
        enable = true;
        package = pkgs.mariadb;
        rootPassword = "<secrets>/mysql_rootPassword";
      };
    }
  ];

  krebs.build.host = config.krebs.hosts.mors;

  networking.wireless.enable = true;

  networking.extraHosts = ''
    213.239.205.240 wohnprojekt-rhh.de
    213.239.205.240 karlaskop.de
    213.239.205.240 makeup.apanowicz.de
    213.239.205.240 pixelpocket.de
    213.239.205.240 reich-gebaeudereinigung.de
    213.239.205.240 o.ubikmedia.de
  '';

  hardware.enableAllFirmware = true;
  nixpkgs.config.allowUnfree = true;

  boot = {
    loader.grub.enable = true;
    loader.grub.version = 2;
    loader.grub.device = "/dev/sda";

    initrd.luks.devices = [ { name = "luksroot"; device = "/dev/sda2"; } ];
    initrd.luks.cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
    initrd.availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" ];
    #kernelModules = [ "kvm-intel" "msr" ];
    kernelModules = [ "msr" ];
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

    "/mnt/backups" = {
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
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="a0:88:b4:29:26:bc", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:0c:a7:63", NAME="et0"
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
    echo 'auto' > '/sys/bus/usb/devices/1-1.3/power/control'

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

  hardware.trackpoint = {
    enable = true;
    sensitivity = 220;
    speed = 0;
    emulateWheel = true;
  };

  services.xserver = {
    videoDriver = "intel";
    vaapiDrivers = [ pkgs.vaapiIntel ];
    deviceSection = ''
      Option "AccelMethod" "sna"
      BusID "PCI:0:2:0"
    '';
  };

  environment.systemPackages = with pkgs; [
    cac-api
    sshpass
    get
    teamspeak_client
    hashPassword
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

  krebs.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-p tcp --dport 8000"; target = "ACCEPT"; precedence = 9001; }
      ];
    };
  };

  #touchpad config
  services.xserver.synaptics = {
    enable = true;
    accelFactor = "0.035";
    additionalOptions = ''
      Option "FingerHigh" "60"
      Option "FingerLow"  "60"
    '';
    tapButtons = false;
    twoFingerScroll = true;
  };

  #for google hangout
  users.extraUsers.gm.extraGroups = [ "audio" "video" ];
}
