{ config, pkgs, ... }:

with import <stockholm/lib>;
{
  imports = [
    <stockholm/lass>
    <stockholm/lass/2configs/hw/x220.nix>
    <stockholm/lass/2configs/boot/stock-x220.nix>

    <stockholm/lass/2configs/mouse.nix>
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/baseX.nix>
    <stockholm/lass/2configs/exim-retiolum.nix>
    <stockholm/lass/2configs/programs.nix>
    <stockholm/lass/2configs/bitcoin.nix>
    <stockholm/lass/2configs/browsers.nix>
    <stockholm/lass/2configs/games.nix>
    <stockholm/lass/2configs/pass.nix>
    <stockholm/lass/2configs/elster.nix>
    <stockholm/lass/2configs/steam.nix>
    <stockholm/lass/2configs/wine.nix>
    <stockholm/lass/2configs/git.nix>
    <stockholm/lass/2configs/virtualbox.nix>
    <stockholm/lass/2configs/fetchWallpaper.nix>
    <stockholm/lass/2configs/mail.nix>
    <stockholm/lass/2configs/repo-sync.nix>
    <stockholm/lass/2configs/ircd.nix>
    <stockholm/lass/2configs/logf.nix>
    <stockholm/lass/2configs/syncthing.nix>
    <stockholm/lass/2configs/otp-ssh.nix>
    {
      #risk of rain port
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp --dport 11100"; target = "ACCEPT"; }
      ];
    }
    {
      services.elasticsearch = {
        enable = true;
      };
    }
    {
      lass.umts = {
        enable = true;
        modem = "/dev/serial/by-id/usb-Lenovo_F5521gw_C12AD95CB7B78F90-if09";
        initstrings = ''
          Init1 = AT+CFUN=1
          Init2 = AT+CGDCONT=1,"IP","pinternet.interkom.de","",0,0
        '';
      };
    }
    {
      services.nginx = {
        enable = true;
        virtualHosts.default = {
          serverAliases = [
            "localhost"
            "${config.krebs.build.host.name}"
            "${config.krebs.build.host.name}.r"
          ];
          locations."~ ^/~(.+?)(/.*)?\$".extraConfig = ''
            alias /home/$1/public_html$2;
          '';
        };
      };
    }
    {
      services.redis.enable = true;
    }
    {
      environment.systemPackages = [
        pkgs.ovh-zone
      ];
    }
    {
      #ps vita stuff
      boot.extraModulePackages = [ config.boot.kernelPackages.exfat-nofuse ];
    }
    {
      services.tor = {
        enable = true;
        client.enable = true;
      };
    }
  ];

  krebs.build.host = config.krebs.hosts.mors;

  fileSystems = {
    "/bku" = {
      device = "/dev/mapper/pool-bku";
      fsType = "btrfs";
      options = ["defaults" "noatime" "ssd" "compress=lzo"];
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="00:24:d7:f0:e8:c8", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:8f:8a:78", NAME="et0"
  '';

  #TODO activationScripts seem broken, fix them!
  #activationScripts
  #split up and move into base
  system.activationScripts.powertopTunables = ''
    #Runtime PMs
    echo 'auto' > '/sys/bus/pci/devices/0000:00:02.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:00.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1f.3/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1f.2/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1f.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1d.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1c.3/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1c.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1b.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1a.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:19.0/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1c.1/power/control'
    echo 'auto' > '/sys/bus/pci/devices/0000:00:1c.4/power/control'
  '';

  environment.systemPackages = with pkgs; [
    acronym
    brain
    cac-api
    sshpass
    get
    teamspeak_client
    hashPassword
    urban
    mk_sql_pair
    remmina

    iodine

    macchanger
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

  krebs.repo-sync.timerConfig = {
    OnCalendar = "00:37";
  };
}
