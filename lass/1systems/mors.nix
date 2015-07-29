{ config, pkgs, ... }:

{
  imports = [
    ../2configs/desktop-base.nix
    ../2configs/programs.nix
    ../2configs/bitcoin.nix
    ../2configs/browsers.nix
    ../2configs/games.nix
    ../2configs/pass.nix
    ../2configs/virtualbox.nix
    ../2configs/elster.nix
    ../2configs/urxvt.nix
    ../2configs/steam.nix
    ../2configs/wine.nix
    ../2configs/texlive.nix
    ../2configs/binary-caches.nix
    ../2configs/ircd.nix
    ../2configs/chromium-patched.nix
    ../2configs/new-repos.nix
    #../../2configs/tv/synaptics.nix
    ../2configs/retiolum.nix
  ];

  krebs.build = {
    user = config.krebs.users.lass;
    target = "root@mors";
    host = config.krebs.hosts.mors;
    deps = {
      nixpkgs = {
        url = https://github.com/Lassulus/nixpkgs;
        rev = "1879a011925c561f0a7fd4043da0768bbff41d0b";
      };
      secrets = {
        url = "/home/lass/secrets/${config.krebs.build.host.name}";
      };
      stockholm = {
        url = toString ../..;
      };
    };
  };

  networking.hostName = "mors";
  networking.wireless.enable = true;

  networking.extraHosts = ''
  '';

  nix.maxJobs = 4;

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
    echo 'auto' > '/sys/bus/usb/devices/1-1.4/power/control'
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

  #system.activationScripts.trackpoint = ''
  #  echo 0 > '/sys/devices/platform/i8042/serio1/serio2/speed'
  #  echo 220 > '/sys/devices/platform/i8042/serio1/serio2/sensitivity'
  #'';

  services.xserver = {
    videoDriver = "intel";
    vaapiDrivers = [ pkgs.vaapiIntel ];
    deviceSection = ''
      Option "AccelMethod" "sna"
      BusID "PCI:0:2:0"
    '';
  };

  environment.systemPackages = with pkgs; [
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

  networking.firewall = {
    allowPing = true;
    allowedTCPPorts = [
      8000
    ];
    allowedUDPPorts = [
      67
    ];
  };

  services.mongodb = {
    enable = true;
  };

  lass.iptables = {
    tables = {
      filter.INPUT.rules = [
        { predicate = "-p tcp --dport 8000"; target = "ACCEPT"; precedence = 9001; }
      ];
    };
  };
}
