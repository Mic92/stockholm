{ config, lib, pkgs, ... }:
with import <stockholm/lib>;

{
  imports = [
    <stockholm/lass>
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
      boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
      users.users.riot = {
        uid = genid "riot";
        isNormalUser = true;
        extraGroups = [ "libvirtd" ];
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQC6o6sdTu/CX1LW2Ff5bNDqGEAGwAsjf0iIe5DCdC7YikCct+7x4LTXxY+nDlPMeGcOF88X9/qFwdyh+9E4g0nUAZaeL14Uc14QDqDt/aiKjIXXTepxE/i4JD9YbTqStAnA/HYAExU15yqgUdj2dnHu7OZcGxk0ZR1OY18yclXq7Rq0Fd3pN3lPP1T4QHM9w66r83yJdFV9szvu5ral3/QuxQnCNohTkR6LoJ4Ny2RbMPTRtb+jPbTQYTWUWwV69mB8ot5nRTP4MRM9pu7vnoPF4I2S5DvSnx4C5zdKzsb7zmIvD4AmptZLrXj4UXUf00Xf7Js5W100Ne2yhYyhq+35 riot@lagrange"
        ];
      };

      # TODO write function for proxy_pass (ssl/nonssl)

      krebs.iptables.tables.filter.FORWARD.rules = [
        { v6 = false; precedence = 1000; predicate = "-d 192.168.122.92"; target = "ACCEPT"; }
      ];
      krebs.iptables.tables.nat.PREROUTING.rules = [
        { v6 = false; precedence = 1000; predicate = "-d 46.4.114.243"; target = "DNAT --to-destination 192.168.122.92"; }
      ];
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
      systemd.services."container@hotdog".reloadIfChanged = mkForce false;
      containers.hotdog = {
        config = { ... }: {
          imports = [ <stockholm/lass/2configs/rebuild-on-boot.nix> ];
          environment.systemPackages = [ pkgs.git ];
          services.openssh.enable = true;
          users.users.root.openssh.authorizedKeys.keys = [
            config.krebs.users.lass.pubkey
          ];
        };
        autoStart = true;
        enableTun = true;
        privateNetwork = true;
        hostAddress = "10.233.2.1";
        localAddress = "10.233.2.2";
      };
    }
    {
      #onondaga
      systemd.services."container@onondaga".reloadIfChanged = mkForce false;
      containers.onondaga = {
        config = { ... }: {
          imports = [ <stockholm/lass/2configs/rebuild-on-boot.nix> ];
          environment.systemPackages = [ pkgs.git ];
          services.openssh.enable = true;
          users.users.root.openssh.authorizedKeys.keys = [
            config.krebs.users.lass.pubkey
            config.krebs.users.nin.pubkey
          ];
        };
        autoStart = true;
        enableTun = true;
        privateNetwork = true;
        hostAddress = "10.233.2.5";
        localAddress = "10.233.2.6";
      };
    }
    <stockholm/lass/2configs/exim-smarthost.nix>
    <stockholm/lass/2configs/ts3.nix>
    <stockholm/lass/2configs/privoxy-retiolum.nix>
    <stockholm/lass/2configs/radio.nix>
    <stockholm/lass/2configs/repo-sync.nix>
    <stockholm/lass/2configs/binary-cache/server.nix>
    <stockholm/lass/2configs/iodined.nix>
    <stockholm/lass/2configs/paste.nix>
    <stockholm/lass/2configs/syncthing.nix>
    <stockholm/lass/2configs/reaktor-coders.nix>
    <stockholm/lass/2configs/ciko.nix>
    <stockholm/lass/2configs/container-networking.nix>
    <stockholm/lass/2configs/monitoring/prometheus-server.nix>
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
      users.users.jeschli = {
        uid = genid "jeschli";
        isNormalUser = true;
        openssh.authorizedKeys.keys = with config.krebs.users; [
          jeschli.pubkey
          jeschli-bln.pubkey
          jeschli-bolide.pubkey
          jeschli-brauerei.pubkey
        ];
      };
      krebs.git.rules = [
        {
          user = with config.krebs.users; [
            jeschli
            jeschli-bln
            jeschli-bolide
            jeschli-brauerei
          ];
          repo = [ config.krebs.git.repos.xmonad-stockholm ];
          perm = with git; push "refs/heads/jeschli*" [ fast-forward non-fast-forward create delete merge ];
        }
        {
          user = with config.krebs.users; [
            jeschli
            jeschli-bln
            jeschli-bolide
            jeschli-brauerei
          ];
          repo = [ config.krebs.git.repos.stockholm ];
          perm = with git; push "refs/heads/staging/jeschli*" [ fast-forward non-fast-forward create delete merge ];
        }
      ];
    }
    {
      krebs.repo-sync.repos.stockholm.timerConfig = {
        OnBootSec = "5min";
        OnUnitInactiveSec = "2min";
        RandomizedDelaySec = "2min";
      };
    }
    <stockholm/lass/2configs/downloading.nix>
    <stockholm/lass/2configs/minecraft.nix>
    {
      services.taskserver = {
        enable = true;
        fqdn = "lassul.us";
        listenHost = "::";
        listenPort = 53589;
        organisations.lass.users = [ "lass" "android" ];
      };
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp --dport 53589"; target = "ACCEPT"; }
      ];
    }
    <stockholm/lass/2configs/go.nix>
    {
      environment.systemPackages = [ pkgs.cryptsetup ];
      systemd.services."container@red".reloadIfChanged = mkForce false;
      containers.red = {
        config = { ... }: {
          environment.systemPackages = [ pkgs.git ];
          services.openssh.enable = true;
          users.users.root.openssh.authorizedKeys.keys = [
            config.krebs.users.lass.pubkey
          ];
        };
        autoStart = false;
        enableTun = true;
        privateNetwork = true;
        hostAddress = "10.233.2.3";
        localAddress = "10.233.2.4";
      };
      services.nginx.virtualHosts."rote-allez-fraktion.de" = {
        enableACME = true;
        forceSSL = true;
        locations."/" = {
          extraConfig = ''
            proxy_set_header Host rote-allez-fraktion.de;
            proxy_pass http://10.233.2.4;
          '';
        };
      };
    }
    {
      imports = [ <stockholm/lass/2configs/backup.nix> ];
      lass.restic = genAttrs [
        "daedalus"
        "icarus"
        "littleT"
        "mors"
        "shodan"
        "skynet"
      ] (dest: {
        dirs = [
          "/home/chat/.weechat"
          "/bku/sql_dumps"
        ];
        passwordFile = (toString <secrets>) + "/restic/${dest}";
        repo = "sftp:backup@${dest}.r:/backups/prism";
        extraArguments = [
          "sftp.command='ssh backup@${dest}.r -i ${config.krebs.build.host.ssh.privkey.path} -s sftp'"
        ];
        timerConfig = {
          OnCalendar = "00:05";
          RandomizedDelaySec = "5h";
        };
      });
    }
    {
      users.users.download.openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDB0d0JA20Vqn7I4lCte6Ne2EOmLZyMJyS9yIKJYXNLjbLwkQ4AYoQKantPBkTxR75M09E7d3j5heuWnCjWH45TrfQfe1EOSSC3ppCI6C6aIVlaNs+KhAYZS0m2Y8WkKn+TT5JLEa8yybYVN/RlZPOilpj/1QgjU6CQK+eJ1k/kK+QFXcwN82GDVh5kbTVcKUNp2tiyxFA+z9LY0xFDg/JHif2ROpjJVLQBJ+YPuOXZN5LDnVcuyLWKThjxy5srQ8iDjoxBg7dwLHjby5Mv41K4W61Gq6xM53gDEgfXk4cQhJnmx7jA/pUnsn2ZQDeww3hcc7vRf8soogXXz2KC9maiq0M/svaATsa9Ul4hrKnqPZP9Q8ScSEAUX+VI+x54iWrnW0p/yqBiRAzwsczdPzaQroUFTBxrq8R/n5TFdSHRMX7fYNOeVMjhfNca/gtfw9dYBVquCvuqUuFiRc0I7yK44rrMjjVQRcAbw6F8O7+04qWCmaJ8MPlmApwu2c05VMv9hiJo5p6PnzterRSLCqF6rIdhSnuOwrUIt1s/V+EEZXHCwSaNLaQJnYL0H9YjaIuGz4c8kVzxw4c0B6nl+hqW5y5/B2cuHiumnlRIDKOIzlv8ufhh21iN7QpIsPizahPezGoT1XqvzeXfH4qryo8O4yTN/PWoA+f7o9POU7L6hQ== lhebendanz@nixos"
      ];
    }
    {
      services.nginx = {
        enable = true;
        virtualHosts."radio.lassul.us" = {
          forceSSL = true;
          enableACME = true;
          locations."/".extraConfig = ''
            proxy_pass http://localhost:8000;
          '';
        };
      };
    }
    {
      lass.nichtparasoup.enable = true;
      services.nginx = {
        enable = true;
        virtualHosts."lol.lassul.us" = {
          forceSSL = true;
          enableACME = true;
          locations."/".extraConfig = ''
            proxy_pass http://localhost:5001;
          '';
        };
      };
    }
    {
      krebs.iptables.tables.filter.INPUT.rules = [
         { predicate = "-p udp --dport 51820"; target = "ACCEPT"; }
      ];
      krebs.iptables.tables.nat.PREROUTING.rules = [
        { v6 = false; precedence = 1000; predicate = "-s 10.244.1.0/24"; target = "ACCEPT"; }
      ];
      krebs.iptables.tables.filter.FORWARD.rules = [
        { v6 = false; precedence = 1000; predicate = "-s 10.244.1.0/24"; target = "ACCEPT"; }
        { v6 = false; precedence = 1000; predicate = "-s 10.243.0.0/16 -d 10.244.1.0/24"; target = "ACCEPT"; }
      ];
      krebs.iptables.tables.nat.POSTROUTING.rules = [
        { v6 = false; predicate = "-s 10.244.1.0/24 ! -d 10.244.1.0/24"; target = "MASQUERADE"; }
      ];
      networking.wireguard.interfaces.wg0 = {
        ips = [ "10.244.1.1/24" ];
        listenPort = 51820;
        privateKeyFile = (toString <secrets>) + "/wireguard.key";
        allowedIPsAsRoutes = true;
        peers = [
          {
            # lass-android
            allowedIPs = [ "10.244.1.2/32" ];
            publicKey = "63+ns9AGv6e6a8WgxiZNFEt1xQT0YKFlEHzRaYJWtmk=";
          }
        ];
      };
    }
    {
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p udp --dport 60000:61000"; target = "ACCEPT";}
      ];
    }
    {
      services.murmur.enable = true;
      services.murmur.registerName = "lassul.us";
      krebs.iptables.tables.filter.INPUT.rules = [
        { predicate = "-p tcp --dport 64738"; target = "ACCEPT";}
      ];

    }
  ];

  krebs.build.host = config.krebs.hosts.prism;
  # workaround because grub store paths are broken
  boot.copyKernels = true;
  services.earlyoom = {
    enable = true;
    freeMemThreshold = 5;
  };
}
