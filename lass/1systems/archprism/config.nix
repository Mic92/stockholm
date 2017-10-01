{ config, lib, pkgs, ... }:
with import <stockholm/lib>;

let
  ip = config.krebs.build.host.nets.internet.ip4.addr;

in {
  imports = [
    <stockholm/lass>
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
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/exim-smarthost.nix>
    #<stockholm/lass/2configs/downloading.nix>
    <stockholm/lass/2configs/ts3.nix>
    <stockholm/lass/2configs/bitlbee.nix>
    <stockholm/lass/2configs/weechat.nix>
    <stockholm/lass/2configs/privoxy-retiolum.nix>
    <stockholm/lass/2configs/radio.nix>
    <stockholm/lass/2configs/repo-sync.nix>
    <stockholm/lass/2configs/binary-cache/server.nix>
    <stockholm/lass/2configs/iodined.nix>
    <stockholm/lass/2configs/libvirt.nix>
    <stockholm/lass/2configs/hfos.nix>
    <stockholm/lass/2configs/monitoring/server.nix>
    <stockholm/lass/2configs/monitoring/monit-alarms.nix>
    <stockholm/lass/2configs/paste.nix>
    <stockholm/lass/2configs/syncthing.nix>
    #<stockholm/lass/2configs/reaktor-coders.nix>
    <stockholm/lass/2configs/ciko.nix>
    <stockholm/lass/2configs/container-networking.nix>
    #<stockholm/lass/2configs/reaktor-krebs.nix>
    #{
    #  lass.pyload.enable = true;
    #}
    {
      imports = [
        <stockholm/lass/2configs/bepasty.nix>
      ];
      krebs.bepasty.servers."paste.r".nginx.extraConfig = ''
        if ( $server_addr = "${config.krebs.build.host.nets.internet.ip4.addr}" ) {
          return 403;
        }
      '';
    }
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

      fileSystems."/tmp" = {
        device = "tmpfs";
        fsType = "tmpfs";
        options = ["nosuid" "nodev" "noatime"];
      };

    }
    {
      sound.enable = false;
    }
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
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDjesiOnhpT9XgWZqw/64M5lVQg3q0k22BtMyCv+33sGX8VmfTyD11GuwSjNGf5WiswKLqFvYBQsHfDDtS3k0ZNTDncGw3Pbilm6QoCuHEyDPaQYin0P+JmkocrL/6QF5uhZVFnsXCH5wntwOa00VFGwpMgQYSfRlReRx42Pu9Jk+iJduZMRBbOMvJI68Z7iJ4DgW/1U9J4MQdCsk7QlFgUstQQfV1zk4VfVfXuxDP3hjx6Q05nDChjpmzJbFunzb7aiy/1/Sl0QhROTpvxrQLksg7yYLw4BRs9ptjehX45A2Sxi8WKOb/g5u3xJNy0X07rE+N+o5v2hS7wF0DLQdK5+4TGtO+Y+ABUCqqA+T1ynAjNBWvsgY5uD4PZjuPgCMSw0JBmIy/P0THi3v5/8Cohvfnspl7Jpf80qENMu3unvvE9EePzgSRZY1PvDjPQfkWy0yBX1yQMhHuVGke9QgaletitwuahRujml37waeUuOl8Rpz+2iV+6OIS4tfO368uLFHKWbobXTbTDXODBgxZ/IyvO7vxM2uDX/kIWaeYKrip3nSyWBYnixwrcS4vm6ZQcoejwp2KCfGQwIE4MnGYRlwcOEYjvyjLkZHDiZEivUQ0rThMYBzec8bQ08QW8oxF+NXkFKG3awt3f7TKTRkYqQcOMpFKmV24KDiwgwm0miQ== JuiceSSH"
      ];
    }
    {
      time.timeZone = "Europe/Berlin";
    }
    {
      imports = [
        <stockholm/lass/2configs/websites/domsen.nix>
        <stockholm/lass/2configs/websites/lassulus.nix>
      ];
      krebs.iptables.tables.filter.INPUT.rules = [
         { predicate = "-p tcp --dport http"; target = "ACCEPT"; }
         { predicate = "-p tcp --dport https"; target = "ACCEPT"; }
      ];
    }
    {
      services.tor = {
        enable = true;
      };
    }
    {
      lass.ejabberd = {
        enable = true;
        hosts = [ "lassul.us" ];
      };
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp --dport xmpp-client"; target = "ACCEPT"; }
        { predicate = "-p tcp --dport xmpp-server"; target = "ACCEPT"; }
      ];
    }
    {
      imports = [
        <stockholm/lass/2configs/realwallpaper.nix>
      ];
      services.nginx.virtualHosts."lassul.us".locations."/wallpaper.png".extraConfig = ''
        alias /var/realwallpaper/realwallpaper.png;
      '';
    }
    {
      environment.systemPackages = with pkgs; [
        mk_sql_pair
      ];
    }
    {
      users.users.tv = {
        uid = genid "tv";
        inherit (config.krebs.users.tv) home;
        group = "users";
        createHome = true;
        useDefaultShell = true;
        openssh.authorizedKeys.keys = [
          config.krebs.users.tv.pubkey
        ];
      };
      users.users.makefu = {
        uid = genid "makefu";
        isNormalUser = true;
        openssh.authorizedKeys.keys = [
          config.krebs.users.makefu.pubkey
        ];
      };
      users.users.nin = {
        uid = genid "nin";
        inherit (config.krebs.users.nin) home;
        group = "users";
        createHome = true;
        useDefaultShell = true;
        openssh.authorizedKeys.keys = [
          config.krebs.users.nin.pubkey
        ];
        extraGroups = [
          "libvirtd"
        ];
      };
    }
    {
      krebs.repo-sync.timerConfig = {
        OnBootSec = "15min";
        OnUnitInactiveSec = "90min";
        RandomizedDelaySec = "30min";
      };
      krebs.repo-sync.repos.stockholm.timerConfig = {
        OnBootSec = "5min";
        OnUnitInactiveSec = "2min";
        RandomizedDelaySec = "2min";
      };
    }
    {
      lass.usershadow = {
        enable = true;
      };
    }
    #{
    #  krebs.Reaktor.prism = {
    #    nickname = "Reaktor|lass";
    #    channels = [ "#retiolum" ];
    #    extraEnviron = {
    #      REAKTOR_HOST = "ni.r";
    #    };
    #    plugins = with pkgs.ReaktorPlugins; [
    #      sed-plugin
    #    ];
    #  };
    #}
    {
      #stuff for dritter
      users.extraUsers.dritter = {
        name = "dritter";
        uid = genid "dritter";
        home = "/home/dritter";
        group = "users";
        createHome = true;
        useDefaultShell = true;
        extraGroups = [
          "download"
        ];
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDnqOWDDk7QkSAvrSLkEoz7dY22+xPyv5JDn2zlfUndfavmTMfZvPx9REMjgULbcCSM4m3Ncf40yUjciDpVleGoEz82+p/ObHAkVWPQyXRS3ZRM2IJJultBHEFc61+61Pi8k3p5pBhPPaig6VncJ4uUuuNqen9jqLesSTVXNtdntU2IvnC8B8k1Kq6fu9q1T2yEOMxkD31D5hVHlqAly0LdRiYvtsRIoCSmRvlpGl70uvPprhQxhtoiEUeDqmIL7BG9x7gU0Swdl7R0/HtFXlFuOwSlNYDmOf/Zrb1jhOpj4AlCliGUkM0iKIJhgH0tnJna6kfkGKHDwuzITGIh6SpZ dritter@Janeway"
        ];
      };
    }
    {
      #hotdog
      containers.hotdog = {
        config = { ... }: {
          services.openssh.enable = true;
          users.users.root.openssh.authorizedKeys.keys = [
            config.krebs.users.lass.pubkey
          ];
        };
        enableTun = true;
        privateNetwork = true;
        hostAddress = "10.233.2.1";
        localAddress = "10.233.2.2";
      };
    }
    {
      #kaepsele
      containers.kaepsele = {
        config = { ... }: {
          services.openssh.enable = true;
          users.users.root.openssh.authorizedKeys.keys = with config.krebs.users; [
            lass.pubkey
            tv.pubkey
          ];
        };
        enableTun = true;
        privateNetwork = true;
        hostAddress = "10.233.2.3";
        localAddress = "10.233.2.4";
      };
    }
    {
      #onondaga
      containers.onondaga = {
        config = { ... }: {
          services.openssh.enable = true;
          users.users.root.openssh.authorizedKeys.keys = [
            config.krebs.users.lass.pubkey
            config.krebs.users.nin.pubkey
          ];
        };
        enableTun = true;
        privateNetwork = true;
        hostAddress = "10.233.2.4";
        localAddress = "10.233.2.5";
      };
    }
  ];

  krebs.build.host = config.krebs.hosts.archprism;
}
