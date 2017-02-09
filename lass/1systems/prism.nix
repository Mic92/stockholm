{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let
  ip = config.krebs.build.host.nets.internet.ip4.addr;

  inherit (import <stockholm/lass/2configs/websites/util.nix> {inherit lib pkgs;})
    manageCerts
  ;

in {
  imports = [
    ../.
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
    ../2configs/retiolum.nix
    ../2configs/exim-smarthost.nix
    ../2configs/downloading.nix
    ../2configs/ts3.nix
    ../2configs/bitlbee.nix
    ../2configs/weechat.nix
    ../2configs/privoxy-retiolum.nix
    ../2configs/radio.nix
    ../2configs/buildbot-standalone.nix
    ../2configs/repo-sync.nix
    ../2configs/binary-cache/server.nix
    ../2configs/iodined.nix
    ../2configs/libvirt.nix
    ../2configs/hfos.nix
    ../2configs/makefu-sip.nix
    ../2configs/monitoring/server.nix
    {
      imports = [
        ../2configs/bepasty.nix
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
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDiupmvj8gmiqIUTk9t4AOZ6bYvIpMMuNZULwIu/lbq8epK+FyvjRtCxqkLgFw0BfNYHzT9fxJ3kZY+nf/pnpI0j8TRwLAyPnfKuBfehrtzjdNbf1jCB9BQAaLoBHkLUFLJxskIC11nHx5KJVJvZBZZ6Odq1WIb4RZXjtEreQfvF8+YyFgHQ/epmQupKK6agHGkjtqvH+hz//dwDqHU9orj8MCWxNaa2wUgn+5laAvpLS15MQeCDIz8GJtJWToETY6bvldiLp2hIAJmgx8LIecV1h7YPR81Rk80gIk3f0PDfsLnnM6ibkI5p8NGh7nRKAdf+W90HpBIHKMRpRkMYvgL3ejVuKJZyzhYKUArA6egNRAN2d67eOpR/yKV5LjRxv+JBCOln5ynDbAmP4Hq98h+0K9Md7VavrRJzzPRTH3MPx+OKqsnRBRjhKsRFIUaO6/TBjZF4RCbbSSbvBW1u+qbTWX1MGUJNB5huL/OIBdHHcTb6GI3W4Svtgq4in2KI4COBhUJogm5UmaXRHtgqvn8byxutIsXMOTFjjdDK6r7mmOj1mzlu5wEHAV6FRsII92pf4WO2GYEUxz1ABu4HvRsVKlvYoEwwBhpXecTve6nogAiDvuxHjUX1eup3If3s4SvNirNPPUQuzNriYJ4JXiU6pGCJcBgxhl+NQ7ajxCdaw== JuiceSSH"
      ];
    }
    {
      time.timeZone = "Europe/Berlin";
    }
    {
      imports = [
        ../2configs/websites/wohnprojekt-rhh.de.nix
        ../2configs/websites/domsen.nix
        ../2configs/websites/lassulus.nix
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
        ../2configs/realwallpaper.nix
      ];
      services.nginx.virtualHosts."lassul.us".locations."/wallpaper.png".extraConfig = ''
        alias /tmp/wallpaper.png;
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
    }
    {
      krebs.repo-sync.timerConfig = {
        OnUnitInactiveSec = "5min";
        RandomizedDelaySec = "2min";
      };
    }
    {
      lass.usershadow = {
        enable = true;
      };
    }
    {
      # Nin stuff
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
      krebs.iptables.tables.nat.PREROUTING.rules = [
        { v6 = false; precedence = 1000; predicate = "-d 213.239.205.240 -p tcp --dport 1337"; target = "DNAT --to-destination 192.168.122.24:22"; }
      ];
      krebs.iptables.tables.filter.FORWARD.rules = [
        { v6 = false; precedence = 1000; predicate = "-d 192.168.122.24 -p tcp --dport 22 -m state --state NEW,ESTABLISHED,RELATED"; target = "ACCEPT"; }
      ];
    }
    {
      krebs.Reaktor.coders = {
        nickname = "reaktor-lass";
        channels = [ "#coders" ];
        extraEnviron = {
          REAKTOR_HOST = "irc.hackint.org";
        };
        plugins = with pkgs.ReaktorPlugins; let
          lambdabotflags = ''
            -XStandaloneDeriving -XGADTs -XFlexibleContexts \
            -XFlexibleInstances -XMultiParamTypeClasses \
            -XOverloadedStrings -XFunctionalDependencies \'';
        in [
          url-title
          (buildSimpleReaktorPlugin "lambdabot-pl" {
            pattern = "^@pl (?P<args>.*)$$";
            script = pkgs.writeDash "lambda-pl" ''
              exec ${pkgs.lambdabot}/bin/lambdabot \
                ${indent lambdabotflags}
                -e "@pl $1"
            '';
          })
          (buildSimpleReaktorPlugin "lambdabot-type" {
            pattern = "^@type (?P<args>.*)$$";
            script = pkgs.writeDash "lambda-type" ''
              exec ${pkgs.lambdabot}/bin/lambdabot \
                ${indent lambdabotflags}
                -e "@type $1"
            '';
          })
          (buildSimpleReaktorPlugin "lambdabot-let" {
            pattern = "^@let (?P<args>.*)$$";
            script = pkgs.writeDash "lambda-let" ''
              exec ${pkgs.lambdabot}/bin/lambdabot \
                ${indent lambdabotflags}
                -e "@let $1"
            '';
          })
          (buildSimpleReaktorPlugin "lambdabot-run" {
            pattern = "^@run (?P<args>.*)$$";
            script = pkgs.writeDash "lambda-run" ''
              exec ${pkgs.lambdabot}/bin/lambdabot \
                ${indent lambdabotflags}
                -e "@run $1"
            '';
          })
          (buildSimpleReaktorPlugin "lambdabot-kind" {
            pattern = "^@kind (?P<args>.*)$$";
            script = pkgs.writeDash "lambda-kind" ''
              exec ${pkgs.lambdabot}/bin/lambdabot \
                ${indent lambdabotflags}
                -e "@kind $1"
            '';
          })
          (buildSimpleReaktorPlugin "lambdabot-kind" {
            pattern = "^@kind (?P<args>.*)$$";
            script = pkgs.writeDash "lambda-kind" ''
              exec ${pkgs.lambdabot}/bin/lambdabot \
                ${indent lambdabotflags}
                -e "@kind $1"
            '';
          })
          (buildSimpleReaktorPlugin "random-unicorn-porn" {
            pattern = "^!rup$$";
            script = pkgs.writePython2 "rup" ''
              #!${pkgs.python2}/bin/python
              t1 = """
                                  _.
                               ;=',_ ()
                     8===D~~  S" .--`||
                             sS  \__ ||
                          __.' ( \-->||
                       _=/    _./-\/ ||
              8===D~~ ((\( /-'   -'l ||
                       ) |/ \\      (_))
                          \\  \\
                           '~ '~
              """
              print(t1)
            '';
          })
          (buildSimpleReaktorPlugin "ping" {
            pattern = "^!ping (?P<args>.*)$$";
            script = pkgs.writeDash "ping" ''
              exec /var/setuid-wrappers/ping -q -c1 "$1" 2>&1 | tail -1
            '';
          })
        ];
      };
    }
  ];

  krebs.build.host = config.krebs.hosts.prism;
}
