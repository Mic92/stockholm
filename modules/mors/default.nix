{ config, pkgs, ... }:

{
  imports = [
    ../lass/desktop-base.nix
    ../lass/programs.nix
    ../lass/retiolum-mors.nix
    ../lass/xserver-lass.nix
    ../tv/synaptics.nix
    ../lass/bitcoin.nix
    ../lass/browsers-lass.nix
    ../lass/games.nix
    ../tv/exim-retiolum.nix
    ../lass/pass.nix
    ../lass/vim.nix
    ../lass/virtualbox.nix
    ../lass/elster.nix
    ../lass/urxvt-lass.nix
    ../lass/steam.nix
  ];

  networking.hostName = "mors";
  networking.wireless.enable = true;

  networking.extraHosts = ''
  '';
  nix.maxJobs = 4;

  hardware.enableAllFirmware = true;
  nixpkgs.config.allowUnfree = true;

  boot = {
    kernelParams = [
      "acpi.brightness_switch_enabled=0"
    ];
    loader.grub.enable = true;
    loader.grub.version = 2;
    loader.grub.device = "/dev/sda";

    initrd.luks.devices = [ { name = "luksroot"; device = "/dev/sda2"; } ];
    initrd.luks.cryptoModules = [ "aes" "sha512" "sha1" "xts" ];
    initrd.availableKernelModules = [ "xhci_hcd" "ehci_pci" "ahci" "usb_storage" ];
    #kernelModules = [ "kvm-intel" "msr" ];
    kernelModules = [ "msr" ];
    extraModprobeConfig = ''
    '';
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
  system.activationScripts.trackpoint = ''
    echo 0 > '/sys/devices/platform/i8042/serio1/serio2/speed'
    echo 220 > '/sys/devices/platform/i8042/serio1/serio2/sensitivity'
  '';

  services.xserver = {

    videoDriver = "intel";
    vaapiDrivers = [ pkgs.vaapiIntel ];
    deviceSection = ''
      Option "AccelMethod" "sna"
      BusID "PCI:0:2:0"
    '';
  };

  #TODO move into modules
  users.extraUsers = {
    root = {
      hashedPassword = "$6$78Zog6OCAQn6tFCO$jpOPksguWEsOPz7u1r6kVApD0Zb2SqjFV8Gn1JCZevcMtBI.jm0CcojXvW.v23xWDt4wZE4KcxFSNWNU.E9ef.";
    };
    #main user
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
      extraGroups = [ "wheel" "audio" ];
      hashedPassword = "$6$78Zog6OCAQn6tFCO$jpOPksguWEsOPz7u1r6kVApD0Zb2SqjFV8Gn1JCZevcMtBI.jm0CcojXvW.v23xWDt4wZE4KcxFSNWNU.E9ef.";
    };
    #miefda-mc-dev user
    miefda = {
      uid = 1338;
      name = "miefda";
      #isNormalUser = true;
      group = "users";
      createHome = true;
      home = "/home/miefda";
      useDefaultShell = true;
      isSystemUser = false;
      description = "miefda-minecraft-dev";
    };
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
        "lass"
        "firefox"
        "chromium"
        "google"
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

  services.ircdHybrid = {
    enable = true;

    description = "local test server";
  };

  #TODO
  #services.urxvtd = {
  #  enable = true;
  #  users = [ "lass" ];
  #  urxvtPackage = pkgs.rxvt_unicode_with-plugins;
  #};

  #system.activationScripts.iptables =
  #  let
  #    log = false;
  #    when = c: f: if c then f else "";
  #  in
  #    ''
  #      ip4tables() { ${pkgs.iptables}/sbin/iptables "$@"; }
  #      ip6tables() { ${pkgs.iptables}/sbin/ip6tables "$@"; }
  #      ipXtables() { ip4tables "$@"; ip6tables "$@"; }

  #      #
  #      # nat
  #      #

  #      # reset tables
  #      ipXtables -t nat -F
  #      ipXtables -t nat -X

  #      #
  #      #ipXtables -t nat -A PREROUTING -j REDIRECT ! -i retiolum -p tcp --dport ssh --to-ports 0
  #      ipXtables -t nat -A PREROUTING -j REDIRECT -p tcp --dport 11423 --to-ports ssh

  #      #
  #      # filter
  #      #

  #      # reset tables
  #      ipXtables -P INPUT DROP
  #      ipXtables -P FORWARD DROP
  #      ipXtables -F
  #      ipXtables -X

  #      # create custom chains
  #      ipXtables -N Retiolum

  #      # INPUT
  #      ipXtables -A INPUT -j ACCEPT -m conntrack --ctstate RELATED,ESTABLISHED
  #      ipXtables -A INPUT -j ACCEPT -i lo
  #      ipXtables -A INPUT -j ACCEPT -p tcp --dport ssh -m conntrack --ctstate NEW
  #      ipXtables -A INPUT -j ACCEPT -p tcp --dport http -m conntrack --ctstate NEW
  #      ipXtables -A INPUT -j ACCEPT -p tcp --dport tinc -m conntrack --ctstate NEW
  #      ipXtables -A INPUT -j ACCEPT -p tcp --dport smtp -m conntrack --ctstate NEW

  #      #mc
  #      ipXtables -A INPUT -j ACCEPT -p tcp --dport 25565
  #      ipXtables -A INPUT -j ACCEPT -p udp --dport 25565

  #      ipXtables -A INPUT -j Retiolum -i retiolum
  #      ${when log "ipXtables -A INPUT -j LOG --log-level info --log-prefix 'INPUT DROP '"}

  #      # FORWARD
  #      ${when log "ipXtables -A FORWARD -j LOG --log-level info --log-prefix 'FORWARD DROP '"}

  #      # Retiolum
  #      ip4tables -A Retiolum -j ACCEPT -p icmp --icmp-type echo-request
  #      ip6tables -A Retiolum -j ACCEPT -p ipv6-icmp -m icmp6 --icmpv6-type echo-request


  #      ${when log "ipXtables -A Retiolum -j LOG --log-level info --log-prefix 'REJECT '"}
  #      ipXtables -A Retiolum -j REJECT -p tcp --reject-with tcp-reset
  #      ip4tables -A Retiolum -j REJECT -p udp --reject-with icmp-port-unreachable
  #      ip4tables -A Retiolum -j REJECT        --reject-with icmp-proto-unreachable
  #      ip6tables -A Retiolum -j REJECT -p udp --reject-with icmp6-port-unreachable
  #      ip6tables -A Retiolum -j REJECT

  #    '';
}
