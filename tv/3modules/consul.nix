{ config, lib, pkgs, ... }:

# if quorum gets lost, then start any node with a config that doesn't contain bootstrap_expect
# but -bootstrap
# TODO consul-bootstrap HOST  that actually does is
# TODO tools to inspect state of a cluster in outage state

with lib;
let
  cfg = config.tv.consul;

  out = {
    options.tv.consul = api;
    config = mkIf cfg.enable (mkMerge [
      imp
      { tv.iptables.input-retiolum-accept-new-tcp = [ "8300" "8301" ]; }
      # TODO udp for 8301
    ]);
  };

  api = {
    enable = mkEnableOption "tv.consul";

    dc = mkOption {
      type = types.label;
    };
    hosts = mkOption {
      type = with types; listOf host;
    };
    encrypt-file = mkOption {
      type = types.str; # TODO path (but not just into store)
      default = toString <secrets/consul-encrypt.json>;
    };
    data-dir = mkOption {
      type = types.str; # TODO path (but not just into store)
      default = "/var/lib/consul";
    };
    self = mkOption {
      type = types.host;
    };
    server = mkOption {
      type = types.bool;
      default = false;
    };
    GOMAXPROCS = mkOption {
      type = types.int;
      default = cfg.self.cores;
    };
  };

  consul-config = {
    datacenter = cfg.dc;
    data_dir = cfg.data-dir;
    log_level = "INFO";
    #node_name =
    server = cfg.server;
    enable_syslog = true;
    retry_join =
      # TODO allow consul in other nets than retiolum [maybe]
      concatMap (host: host.nets.retiolum.addrs)
                (filter (host: host.name != cfg.self.name) cfg.hosts);
    leave_on_terminate = true;
  } // optionalAttrs cfg.server {
    bootstrap_expect = length cfg.hosts;
    leave_on_terminate = false;
  };

  imp = {
    environment.systemPackages = with pkgs; [
      consul
    ];

    systemd.services.consul = {
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = with pkgs; [
        consul
      ];
      environment = {
        GOMAXPROCS = toString cfg.GOMAXPROCS;
      };
      serviceConfig = {
        PermissionsStartOnly = "true";
        SyslogIdentifier = "consul";
        User = user.name;
        PrivateTmp = "true";
        Restart = "always";
        ExecStartPre = pkgs.writeScript "consul-init" ''
          #! /bin/sh
          mkdir -p ${cfg.data-dir}
          chown ${user.name}: ${cfg.data-dir}
          install -o ${user.name} -m 0400 ${cfg.encrypt-file} /tmp/encrypt.json
        '';
        ExecStart = pkgs.writeScript "consul-service" ''
          #! /bin/sh
          set -euf
          exec >/dev/null
          exec consul agent \
            -config-file=${toFile "consul.json" (toJSON consul-config)} \
            -config-file=/tmp/encrypt.json
        '';
        #-node=${cfg.self.fqdn} \
        #ExecStart = "${tinc}/sbin/tincd -c ${confDir} -d 0 -U ${user} -D";
      };
    };

    users.extraUsers = singleton {
      inherit (user) name uid;
    };
  };

  user = rec {
    name = "consul";
    uid = genid name;
  };

in
out
