{ config, lib, pkgs, ... }: let
  cfg = config.lass.sync-containers3;
  slib = pkgs.stockholm.lib;
in {
  options.lass.sync-containers3 = {
    inContainer = {
      enable = lib.mkEnableOption "container config for syncing";
      pubkey = lib.mkOption {
        type = lib.types.str; # TODO ssh key
      };
    };
    containers = lib.mkOption {
      default = {};
      type = lib.types.attrsOf (lib.types.submodule ({ config, ... }: {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
            default = config._module.args.name;
          };
          sshKey = lib.mkOption {
            type = slib.types.absolute-pathname;
          };
          luksKey = lib.mkOption {
            type = slib.types.absolute-pathname;
            default = config.sshKey;
          };
          ephemeral = lib.mkOption {
            type = lib.types.bool;
            default = false;
          };
        };
      }));
    };
  };
  config = lib.mkMerge [
    (lib.mkIf (cfg.containers != {}) {

      containers = lib.mapAttrs' (n: ctr: lib.nameValuePair ctr.name {
        config = {
          environment.systemPackages = [
            pkgs.dhcpcd
            pkgs.git
            pkgs.jq
          ];
          networking.useDHCP = lib.mkForce true;
          systemd.services.autoswitch = {
            environment = {
              NIX_REMOTE = "daemon";
            };
            wantedBy = [ "multi-user.target" ];
            serviceConfig.ExecStart = pkgs.writers.writeDash "autoswitch" ''
              set -efu
              ln -frs /var/state/var_src /var/src
              if test -e /var/src/nixos-config; then
                /run/current-system/sw/bin/nixos-rebuild -I /var/src switch || :
              fi
            '';
            unitConfig.X-StopOnRemoval = false;
          };
        };
        autoStart = false;
        enableTun = true;
        ephemeral = ctr.ephemeral;
        privateNetwork = true;
        hostBridge = "ctr0";
        bindMounts = {
          "/etc/resolv.conf".hostPath = "/etc/resolv.conf";
          "/var/lib/self/disk" = {
            hostPath = "/var/lib/sync-containers3/${ctr.name}/disk";
            isReadOnly = false;
          };
          "/var/state" = {
            hostPath = "/var/lib/sync-containers3/${ctr.name}/state";
            isReadOnly = false;
          };
        };
      }) cfg.containers;

      systemd.services = lib.foldr lib.recursiveUpdate {} (lib.flatten (map (ctr: [
        { "${ctr.name}_syncer" = {
          path = with pkgs; [
            coreutils
            consul
            rsync
            openssh
            systemd
          ];
          startAt = "*:0/1";
          serviceConfig = {
            User = "${ctr.name}_container";
            LoadCredential = [
              "ssh_key:${ctr.sshKey}"
            ];
            ExecCondition = pkgs.writers.writeDash "${ctr.name}_checker" ''
              set -efu
              ! systemctl is-active --quiet container@${ctr.name}.service
            '';
            ExecStart = pkgs.writers.writeDash "${ctr.name}_syncer" ''
              set -efux
              consul lock sync_${ctr.name} ${pkgs.writers.writeDash "${ctr.name}-sync" ''
                set -efux
                if ping -c 1 ${ctr.name}.r; then
                  touch "$HOME"/incomplete
                  rsync -a -e "ssh -i $CREDENTIALS_DIRECTORY/ssh_key" --inplace container_sync@${ctr.name}.r:disk "$HOME"/disk
                  rm "$HOME"/incomplete
                fi
              ''}
            '';
          };
        }; }
        { "${ctr.name}_scheduler" = {
          wantedBy = [ "multi-user.target" ];
          path = with pkgs; [
            coreutils
            consul
            cryptsetup
            mount
            util-linux
            systemd
            retry
          ];
          serviceConfig = let
            containerDirectory = lib.removeSuffix "/%i" config.systemd.services."container@${ctr.name}".environment.root;
          in {
            Restart = "always";
            RestartSec = "5s";
            ExecStart = "${pkgs.consul}/bin/consul lock -verbose -monitor-retry 100 container_${ctr.name} ${pkgs.writers.writeBash "${ctr.name}-start" ''
              set -efux
              if test -e /var/lib/sync-containers3/${ctr.name}/incomplete; then
                echo 'data is inconistent, start aborted'
                exit 1
              fi
              trap ${pkgs.writers.writeDash "stop-${ctr.name}" ''
                set -efux
                /run/current-system/sw/bin/nixos-container stop ${ctr.name} || :
                umount /var/lib/sync-containers3/${ctr.name}/state || :
                cryptsetup luksClose ${ctr.name} || :
              ''} INT TERM EXIT
              consul kv put containers/${ctr.name}/host ${config.networking.hostName}
              cryptsetup luksOpen --key-file ${ctr.luksKey} /var/lib/sync-containers3/${ctr.name}/disk ${ctr.name}
              mkdir -p /var/lib/sync-containers3/${ctr.name}/state
              mount /dev/mapper/${ctr.name} /var/lib/sync-containers3/${ctr.name}/state
              /run/current-system/sw/bin/nixos-container start ${ctr.name}
              set +x
              until /run/wrappers/bin/ping -q -c 1 ${ctr.name}.r > /dev/null; do sleep 5; done
              while retry -t 5 -d 60 -- /run/wrappers/bin/ping -q -c 3 ${ctr.name}.r > /dev/null; do sleep 5; done
              echo "lost tinc connection to container, shutting down"
            ''}";
          };
        }; }
      ]) (lib.attrValues cfg.containers)));

      systemd.timers = lib.mapAttrs' (n: ctr: lib.nameValuePair "${ctr.name}_syncer" {
        timerConfig = {
          RandomizedDelaySec = 100;
        };
      }) cfg.containers;

      users.groups = lib.mapAttrs' (_: ctr: lib.nameValuePair "${ctr.name}_container" {
      }) cfg.containers;
      users.users = lib.mapAttrs' (_: ctr: lib.nameValuePair "${ctr.name}_container" ({
        group = "container_${ctr.name}";
        isNormalUser = true;
        uid = slib.genid_uint31 "container_${ctr.name}";
        home = "/var/lib/sync-containers3/${ctr.name}";
        createHome = true;
        homeMode = "705";
      })) cfg.containers;

    })
    (lib.mkIf (cfg.containers != {}) {
      # networking
      networking.networkmanager.unmanaged = [ "ctr0" ];
      networking.interfaces.dummy0.virtual = true;
      networking.bridges.ctr0.interfaces = [ "dummy0" ];
      networking.interfaces.ctr0.ipv4.addresses = [{
        address = "10.233.0.1";
        prefixLength = 24;
      }];
      systemd.services."dhcpd-ctr0" = {
        wantedBy = [ "multi-user.target" ];
        after = [ "network.target" ];
        serviceConfig = {
          Type = "forking";
          Restart = "always";
          DynamicUser = true;
          StateDirectory = "dhcpd-ctr0";
          User = "dhcpd-ctr0";
          Group = "dhcpd-ctr0";
          AmbientCapabilities = [
           "CAP_NET_RAW"          # to send ICMP messages
           "CAP_NET_BIND_SERVICE" # to bind on DHCP port (67)
          ];
          ExecStartPre = "${pkgs.coreutils}/bin/touch /var/lib/dhcpd-ctr0/dhcpd.leases";
          ExecStart = "${pkgs.dhcp}/bin/dhcpd -4 -lf /var/lib/dhcpd-ctr0/dhcpd.leases -cf ${pkgs.writeText "dhpd.conf" ''
            default-lease-time 600;
            max-lease-time 7200;
            authoritative;
            ddns-update-style interim;
            log-facility local1; # see dhcpd.nix

            option subnet-mask 255.255.255.0;
            option routers 10.233.0.1;
            # option domain-name-servers 8.8.8.8; # TODO configure dns server
            subnet 10.233.0.0 netmask 255.255.255.0 {
              range 10.233.0.10 10.233.0.250;
            }
          ''} ctr0";
        };
      };
    })
    (lib.mkIf cfg.inContainer.enable {
      users.groups.container_sync = {};
      users.users.container_sync = {
        group = "container_sync";
        uid = slib.genid_uint31 "container_sync";
        isNormalUser = true;
        home = "/var/lib/self";
        createHome = true;
        openssh.authorizedKeys.keys = [
          cfg.inContainer.pubkey
        ];
      };
    })
  ];
}
