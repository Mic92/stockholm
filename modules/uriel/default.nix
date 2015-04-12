  { config, pkgs, ... }:

  {
    imports = [
      ../lass/desktop-base.nix
      ../lass/retiolum-uriel.nix
      ../lass/xserver-lass.nix
      ../lass/browsers-lass.nix
      ../lass/programs.nix
      ../lass/games.nix
      ../tv/exim-retiolum.nix
      ../lass/pass.nix
      ../lass/vim.nix
      ../lass/urxvt-lass.nix
  ];

  networking.hostName = "uriel";
  networking.wireless.enable = true;
  nix.maxJobs = 2;

  hardware.enableAllFirmware = true;
  nixpkgs.config.allowUnfree = true;

  boot = {
    #kernelParams = [
    #  "acpi.brightness_switch_enabled=0"
    #];
    #loader.grub.enable = true;
    #loader.grub.version = 2;
    #loader.grub.device = "/dev/sda";

    loader.gummiboot.enable = true;
    loader.gummiboot.timeout = 5;

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
      device = "/dev/pool/root";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/sda1";
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="64:27:37:7d:d8:ae", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:b8:c8:2e", NAME="et0"
  '';

  #services.xserver = {
  #};

  services.xserver.synaptics = {
    enable = true;
    twoFingerScroll = true;
    accelFactor = "0.035";
    additionalOptions = ''
      Option "FingerHigh" "60"
      Option "FingerLow"  "60"
    '';
  };

  users.extraUsers = {
    root = {
      hashedPassword = "$6$78Zog6OCAQn6tFCO$jpOPksguWEsOPz7u1r6kVApD0Zb2SqjFV8Gn1JCZevcMtBI.jm0CcojXvW.v23xWDt4wZE4KcxFSNWNU.E9ef.";
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
      extraGroups = [ "wheel" "audio" ];
      hashedPassword = "$6$78Zog6OCAQn6tFCO$jpOPksguWEsOPz7u1r6kVApD0Zb2SqjFV8Gn1JCZevcMtBI.jm0CcojXvW.v23xWDt4wZE4KcxFSNWNU.E9ef.";
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAp83zynhIueJJsWlSEykVSBrrgBFKq38+vT8bRfa+csqyjZBl2SQFuCPo+Qbh49mwchpZRshBa9jQEIGqmXxv/PYdfBFQuOFgyUq9ZcTZUXqeynicg/SyOYFW86iiqYralIAkuGPfQ4howLPVyjTZtWeEeeEttom6p6LMY5Aumjz2em0FG0n9rRFY2fBzrdYAgk9C0N6ojCs/Gzknk9SGntA96MDqHJ1HXWFMfmwOLCnxtE5TY30MqSmkrJb7Fsejwjoqoe9Y/mCaR0LpG2cStC1+37GbHJNH0caCMaQCX8qdfgMVbWTVeFWtV6aWOaRgwLrPDYn4cHWQJqTfhtPrNQ== lass@mors"
      ];
    };
  };

  environment.systemPackages = with pkgs; [
  ];

  #users.extraGroups = {
  #  loot = {
  #    members = [
  #      "lass"
  #      "firefox"
  #      "chromium"
  #      "google"
  #    ];
  #  };
  #};
  #
  # iptables
  #
  #networking.firewall.enable = false;
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
