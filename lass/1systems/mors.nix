{ config, pkgs, ... }:

with import <stockholm/lib>;
{
  imports = [
    ../.
    ../2configs/retiolum.nix
    ../2configs/hw/tp-x220.nix
    ../2configs/baseX.nix
    ../2configs/exim-retiolum.nix
    ../2configs/programs.nix
    ../2configs/bitcoin.nix
    ../2configs/browsers.nix
    ../2configs/games.nix
    ../2configs/pass.nix
    ../2configs/elster.nix
    ../2configs/steam.nix
    ../2configs/wine.nix
    ../2configs/git.nix
    ../2configs/libvirt.nix
    ../2configs/fetchWallpaper.nix
    #../2configs/c-base.nix
    ../2configs/mail.nix
    ../2configs/krebs-pass.nix
    ../2configs/repo-sync.nix
    ../2configs/ircd.nix
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
    {
      #zalando project
      services.postgresql = {
        enable = true;
        package = pkgs.postgresql;
      };
      virtualisation.docker.enable = true;
      #users.users.mainUser.extraGroups = [ "docker" ];
    }
    {
      lass.umts = {
        enable = true;
        modem = "/dev/serial/by-id/usb-Lenovo_F5521gw_38214921FBBBC7B0-if09";
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
            "${config.krebs.build.host.name}.retiolum"
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
      virtualisation.libvirtd.enable = true;
    }
    {
      services.nginx = {
        enable = mkDefault true;
        virtualHosts = {
          "stats.mors" = {
            locations = {
              "/"  = {
                proxyPass  = "http://localhost:3000/";
                extraConfig = ''
                  proxy_set_header   Host             $host;
                  proxy_set_header   X-Real-IP        $remote_addr;
                  proxy_set_header   X-Forwarded-For  $proxy_add_x_forwarded_for;
                '';
              };
            };
          };
        };
      };

      services.grafana = {
        enable = true;
        addr = "127.0.0.1";
        users.allowSignUp = false;
        users.allowOrgCreate = false;
        users.autoAssignOrg = false;
        auth.anonymous.enable = true;
        security = import <secrets/grafana_security.nix>; # { AdminUser = ""; adminPassword = ""}
      };

      services.graphite = {
        api = {
          enable = true;
          listenAddress = "127.0.0.1";
          port = 18080;
        };
        carbon = {
          enableCache = true;
          # save disk usage by restricting to 1 bulk update per second
          config = ''
            [cache]
            MAX_CACHE_SIZE = inf
            MAX_UPDATES_PER_SECOND = 1
            MAX_CREATES_PER_MINUTE = 500
            '';
          storageSchemas = ''
            [carbon]
            pattern = ^carbon\.
            retentions = 60:90d

            [elchos]
            patterhn = ^elchos\.
            retentions = 10s:30d,60s:3y

            [default]
            pattern = .*
            retentions = 30s:30d,300s:1y
            '';
        };
      };

      services.collectd = {
        enable = true;
        include = [ (toString (pkgs.writeText "collectd-graphite-cfg" ''
          LoadPlugin write_graphite
          <Plugin "write_graphite">
            <Carbon>
              Host "localhost"
              Port "2003"
              EscapeCharacter "_"
              StoreRates false
              AlwaysAppendDS false
            </Carbon>
          </Plugin>
        ''))
        ];
        extraConfig = ''
          LoadPlugin interface
          LoadPlugin battery
          LoadPlugin load
          LoadPlugin cpu
          LoadPlugin entropy
          LoadPlugin write_graphite
          <Plugin "interface">
            Interface "et0"
            Interface "wl0"
            Interface "retiolum"
          </Plugin>
        '';
      };
      services.graphite.beacon = {
        enable = true;
        config = {
          graphite_url = "http://localhost:18080";
          cli = {
            command = ''${pkgs.irc-announce}/bin/irc-announce irc.freenode.org 6667 mors-beacon-alert \#krebs ' ''${level} ''${name} ''${value}' '';
          };
          smtp = {
            from = "beacon@mors.r";
            to = [
              "lass@mors.r"
            ];
          };
          normal_handlers = [
            "smtp"
            "cli"
          ];
          warning_handlers = [
            "smtp"
            "cli"
          ];
          critical_handlers = [
            "smtp"
            "cli"
          ];
          alerts = [
            {
              name = "testbattery";
              query = "*.battery-0.capacity";
              method = "last_value";
              interval = "1minute";
              logging = "info";
              repeat_interval = "5minute";
              rules = [
                "warning: < 30.0"
                "critical: < 10.0"
              ];
            }
          ];
        };
      };
    }
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
    "/tmp" = {
      device = "tmpfs";
      fsType = "tmpfs";
      options = ["nosuid" "nodev" "noatime"];
    };
  };

  services.udev.extraRules = ''
    SUBSYSTEM=="net", ATTR{address}=="00:24:d7:f0:a0:0c", NAME="wl0"
    SUBSYSTEM=="net", ATTR{address}=="f0:de:f1:71:cb:35", NAME="et0"
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
    exfat

    acronym
    cac-api
    sshpass
    get
    teamspeak_client
    hashPassword
    urban
    mk_sql_pair
    remmina
    thunderbird

    logf
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
