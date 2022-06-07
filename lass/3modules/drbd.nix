{ config, lib, pkgs, ... }: let
  cfg = config.lass.drbd;
  slib = import <stockholm/lib>;
in {
  options = {
    lass.drbd = lib.mkOption {
      default = {};
      type = lib.types.attrsOf (lib.types.submodule ({ config, ... }: {
        options = {
          name = lib.mkOption {
            type = lib.types.str;
            default = config._module.args.name;
          };
          blockMinor = lib.mkOption {
            type = lib.types.int;
            default = lib.mod (slib.genid config.name) 16000; # TODO get max_id fron drbd
          };
          port = lib.mkOption {
            type = lib.types.int;
            default = 20000 + config.blockMinor;
          };
          peers = lib.mkOption {
            type = lib.types.listOf slib.types.host;
          };
          disk = lib.mkOption {
            type = lib.types.str;
            default = "/dev/loop${toString config.blockMinor}";
          };
          drbdConfig = lib.mkOption {
            type = lib.types.path;
            internal = true;
            default = pkgs.writeText "drbd-${config.name}.conf" ''
              resource ${config.name} {
                net {
                  protocol a;
                  ping-int 10;
                }
                device minor ${toString config.blockMinor};
                disk ${config.disk};
                meta-disk internal;
                ${slib.indent (lib.concatStrings (lib.imap1 (i: peer: /* shell */ ''
                  on ${peer.name} {
                    address ${peer.nets.retiolum.ip4.addr}:${toString config.port};
                    node-id ${toString i};
                  }
                '') config.peers))}
                connection-mesh {
                  hosts ${lib.concatMapStringsSep " " (peer: peer.name) config.peers};
                }
              }
            '';
          };
        };
      }));
    };
  };
  config = lib.mkIf (cfg != {}) {
    boot.extraModulePackages = [
      (pkgs.linuxPackages.callPackage ../5pkgs/drbd9/default.nix {})
    ];
    boot.extraModprobeConfig = ''
      options drbd usermode_helper=/run/current-system/sw/bin/drbdadm
    '';
    services.udev.packages = [ pkgs.drbd ];
    boot.kernelModules = [ "drbd" ];

    environment.systemPackages = [ pkgs.drbd ];


    networking.firewall.allowedTCPPorts = map (device: device.port) (lib.attrValues cfg);
    systemd.services = lib.mapAttrs' (_: device:
      lib.nameValuePair "drbd-${device.name}" {
        after = [ "systemd-udev.settle.service" "network.target" ];
        wants = [ "systemd-udev.settle.service" ];
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          RemainAfterExit = true;
          ExecStart = pkgs.writers.writeDash "start-drbd-${device.name}" ''
            set -efux
            mkdir -p /var/lib/sync-containers2
            ${lib.optionalString (device.disk == "/dev/loop${toString device.blockMinor}") ''
              if ! test -e /var/lib/sync-containers2/${device.name}.disk; then
                truncate -s 10G /var/lib/sync-containers2/${device.name}.disk
              fi
              if ! ${pkgs.util-linux}/bin/losetup /dev/loop${toString device.blockMinor}; then
                ${pkgs.util-linux}/bin/losetup /dev/loop${toString device.blockMinor} /var/lib/sync-containers2/${device.name}.disk
              fi
            ''}
            if ! ${pkgs.drbd}/bin/drbdadm adjust ${device.name}; then
              ${pkgs.drbd}/bin/drbdadm down ${device.name}
              ${pkgs.drbd}/bin/drbdadm create-md ${device.name}
              ${pkgs.drbd}/bin/drbdadm up ${device.name}
            fi
          '';
          ExecStop = pkgs.writers.writeDash "stop-drbd-${device.name}" ''
            set -efux
            ${pkgs.drbd}/bin/drbdadm -c ${device.drbdConfig} down ${device.name}
            ${lib.optionalString (device.disk == "/dev/loop${toString device.blockMinor}") ''
              ${pkgs.util-linux}/bin/losetup -d /dev/loop${toString device.blockMinor}
            ''}
          '';
        };
      }
    ) cfg;


    environment.etc."drbd.conf".text = ''
      global {
        usage-count yes;
      }

      ${lib.concatMapStrings (device: /* shell */ ''
        include ${device.drbdConfig};
      '') (lib.attrValues cfg)}
    '';
  };
}

