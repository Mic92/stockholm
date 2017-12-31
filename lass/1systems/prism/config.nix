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
          prefixLength = 27;
        }
      ];
      networking.defaultGateway = "46.4.114.225";
      networking.nameservers = [
        "8.8.8.8"
      ];
      services.udev.extraRules = ''
        SUBSYSTEM=="net", ATTR{address}=="08:60:6e:e7:87:04", NAME="et0"
      '';
    }
    {
      imports = [ <nixpkgs/nixos/modules/installer/scan/not-detected.nix> ];

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
        "ahci" "sd_mod"
      ];

      boot.kernelModules = [ "kvm-intel" ];

      fileSystems."/" = {
        device = "/dev/pool/nix_root";
        fsType = "ext4";
      };

      fileSystems."/tmp" = {
        device = "tmpfs";
        fsType = "tmpfs";
        options = ["nosuid" "nodev" "noatime"];
      };

      fileSystems."/var/download" = {
        device = "/dev/pool/download";
        fsType = "ext4";
      };

      fileSystems."/srv/http" = {
        device = "/dev/pool/http";
        fsType = "ext4";
      };

      fileSystems."/home" = {
        device = "/dev/pool/home";
        fsType = "ext4";
      };

      fileSystems."/bku" = {
        device = "/dev/pool/bku";
        fsType = "ext4";
      };

      swapDevices = [
        { label = "swap1"; }
        { label = "swap2"; }
      ];

      sound.enable = false;
      nixpkgs.config.allowUnfree = true;
      time.timeZone = "Europe/Berlin";
    }
    <stockholm/lass/2configs/retiolum.nix>
    <stockholm/lass/2configs/libvirt.nix>
    {
      services.nginx.enable = true;
      imports = [
        <stockholm/lass/2configs/websites/domsen.nix>
        <stockholm/lass/2configs/websites/lassulus.nix>
      ];
      # needed by domsen.nix ^^
      lass.usershadow = {
        enable = true;
      };

      krebs.iptables.tables.filter.INPUT.rules = [
         { predicate = "-p tcp --dport http"; target = "ACCEPT"; }
         { predicate = "-p tcp --dport https"; target = "ACCEPT"; }
      ];
    }
    { # TODO make new hfos.nix out of this vv
      users.users.riot = {
        uid = genid "riot";
        isNormalUser = true;
        extraGroups = [ "libvirtd" ];
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC6o6sdTu/CX1LW2Ff5bNDqGEAGwAsjf0iIe5DCdC7YikCct+7x4LTXxY+nDlPMeGcOF88X9/qFwdyh+9E4g0nUAZaeL14Uc14QDqDt/aiKjIXXTepxE/i4JD9YbTqStAnA/HYAExU15yqgUdj2dnHu7OZcGxk0ZR1OY18yclXq7Rq0Fd3pN3lPP1T4QHM9w66r83yJdFV9szvu5ral3/QuxQnCNohTkR6LoJ4Ny2RbMPTRtb+jPbTQYTWUWwV69mB8ot5nRTP4MRM9pu7vnoPF4I2S5DvSnx4C5zdKzsb7zmIvD4AmptZLrXj4UXUf00Xf7Js5W100Ne2yhYyhq+35 riot@lagrange"
        ];
      };

      # TODO write function for proxy_pass (ssl/nonssl)
      services.nginx.virtualHosts."hackerfleet.de" = {
        serverAliases = [
          "*.hackerfleet.de"
        ];
        locations."/".extraConfig = ''
          proxy_pass http://192.168.122.92:80;
        '';
      };
      services.nginx.virtualHosts."hackerfleet.de-s" = {
        serverName = "hackerfleet.de";
        listen = [
          {
            addr = "0.0.0.0";
            port = 443;
          }
        ];
        serverAliases = [
          "*.hackerfleet.de"
        ];
        locations."/".extraConfig = ''
          proxy_pass http://192.168.122.92:443;
        '';
      };
    }
    {
      users.users.tv = {
        uid = genid "tv";
        isNormalUser = true;
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
        isNormalUser = true;
        openssh.authorizedKeys.keys = [
          config.krebs.users.nin.pubkey
        ];
      };
      users.extraUsers.dritter = {
        uid = genid "dritter";
        isNormalUser = true;
        extraGroups = [
          "download"
        ];
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDnqOWDDk7QkSAvrSLkEoz7dY22+xPyv5JDn2zlfUndfavmTMfZvPx9REMjgULbcCSM4m3Ncf40yUjciDpVleGoEz82+p/ObHAkVWPQyXRS3ZRM2IJJultBHEFc61+61Pi8k3p5pBhPPaig6VncJ4uUuuNqen9jqLesSTVXNtdntU2IvnC8B8k1Kq6fu9q1T2yEOMxkD31D5hVHlqAly0LdRiYvtsRIoCSmRvlpGl70uvPprhQxhtoiEUeDqmIL7BG9x7gU0Swdl7R0/HtFXlFuOwSlNYDmOf/Zrb1jhOpj4AlCliGUkM0iKIJhgH0tnJna6kfkGKHDwuzITGIh6SpZ dritter@Janeway"
        ];
      };
      users.extraUsers.juhulian = {
        uid = 1339;
        isNormalUser = true;
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDBQhLGvfv4hyQ/nqJGy1YgHXPSVl6igeWTroJSvAhUFgoh+rG+zvqY0EahKXNb3sq0/OYDCTJVuucc0hgCg7T2KqTqMtTb9EEkRmCFbD7F7DWZojCrh/an6sHneqT5eFvzAPZ8E5hup7oVQnj5P5M3I9keRHBWt1rq6q0IcOEhsFvne4qJc73aLASTJkxzlo5U8ju3JQOl6474ECuSn0lb1fTrQ/SR1NgF7jV11eBldkS8SHEB+2GXjn4Yrn+QUKOnDp+B85vZmVlJSI+7XR1/U/xIbtAjGTEmNwB6cTbBv9NCG9jloDDOZG4ZvzzHYrlBXjaigtQh2/4mrHoKa5eV juhulian@juhulian"
        ];
      };
      users.users.hellrazor = {
        uid = genid "hellrazor";
        isNormalUser = true;
        extraGroups = [
          "download"
        ];
        openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDQFaYOWRUvHP6I37q9Dd4PJOq8FNQqAeJZ8pLx0G62uC450kbPGcG80rHHvXmk7HqQP6biJmMg48bOsvXAScPot2Qhp1Qc35CuUqVhLiTvUAsi8l/iJjhjZ23yRGDCAmW5+JIOzIvECkcbMnG7YoYAQ9trNGHe9qwGzQGhpt3QVClE23WtE3PVKRLQx1VbiabSnAm6tXVd2zpUoSdpWt8Gpi2taM4XXJ5+l744MNxFHvDapN5xqpYzwrA34Ii13jNLWcGbtgxESpR+VjnamdWByrkBsW4X5/xn2K1I1FrujaM/DBHV1QMaDKst9V8+uL5X7aYNt0OUBu2eyZdg6aujY2BYovB9uRyR1JIuSbA/a54MM96yN9WirMUufJF/YZrV0L631t9EW8ORyWUo1GRzMuBHVHQlfApj7NCU/jEddUuTqKgwyRgTmMFMUI4M0tRULAB/7pBE1Vbcx9tg6RsKIk8VkskfbBJW9Y6Sx6YoFlxPdgMNIrBefqEjIV62piP7YLMlvfIDCJ7TNd9dLN86XGggZ/nD5zt6SL1o61vVnw9If8pHosppxADPJsJvcdN6fOe16/tFAeE0JRo0jTcyFVTBGfhpey+rFfuW8wtUyuO5WPUxkOn7xMHGMWHJAtWX2vwVIDtLxvqn48B4SmEOpPD6ii+vcpwqAex3ycqBUQ==" ];
      };
    }
    {
      #hotdog
      containers.hotdog = {
        config = { ... }: {
          environment.systemPackages = [ pkgs.git ];
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
          environment.systemPackages = [ pkgs.git ];
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
          environment.systemPackages = [ pkgs.git ];
          services.openssh.enable = true;
          users.users.root.openssh.authorizedKeys.keys = [
            config.krebs.users.lass.pubkey
            config.krebs.users.nin.pubkey
          ];
        };
        enableTun = true;
        privateNetwork = true;
        hostAddress = "10.233.2.5";
        localAddress = "10.233.2.6";
      };
    }
    <stockholm/lass/2configs/exim-smarthost.nix>
    <stockholm/lass/2configs/ts3.nix>
    <stockholm/lass/2configs/IM.nix>
    <stockholm/lass/2configs/privoxy-retiolum.nix>
    <stockholm/lass/2configs/radio.nix>
    <stockholm/lass/2configs/repo-sync.nix>
    <stockholm/lass/2configs/binary-cache/server.nix>
    <stockholm/lass/2configs/iodined.nix>
    <stockholm/lass/2configs/monitoring/server.nix>
    <stockholm/lass/2configs/monitoring/monit-alarms.nix>
    <stockholm/lass/2configs/paste.nix>
    <stockholm/lass/2configs/syncthing.nix>
    <stockholm/lass/2configs/reaktor-coders.nix>
    <stockholm/lass/2configs/ciko.nix>
    <stockholm/lass/2configs/container-networking.nix>
    { # quasi bepasty.nix
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
      services.minecraft-server.enable = true;
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp --dport 25565"; target = "ACCEPT"; }
        { predicate = "-p udp --dport 25565"; target = "ACCEPT"; }
      ];
    }
    <stockholm/krebs/2configs/reaktor-krebs.nix>
    <stockholm/lass/2configs/dcso-dev.nix>
    {
      krebs.git.rules = [
        {
          user = with config.krebs.users; [
            jeschli
            jeschli-bln
            jeschli-brauerei
          ];
          repo = [ config.krebs.git.repos.stockholm ];
          perm = with git; push "refs/heads/staging/jeschli" [ fast-forward non-fast-forward create delete merge ];
        }
      ];
    }
  ];

  krebs.build.host = config.krebs.hosts.prism;
  # workaround because grub store paths are broken
  boot.copyKernels = true;
}
