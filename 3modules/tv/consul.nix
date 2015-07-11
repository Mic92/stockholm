{ config, lib, pkgs, ... }:

# if quorum gets lost, then start any node with a config that doesn't contain bootstrap_expect
# but -bootstrap
# TODO consul-bootstrap HOST  that actually does is
# TODO tools to inspect state of a cluster in outage state

with builtins;
with lib;
let
  cfg = config.tv.consul;

  out = {
    imports = [ ../../3modules/tv/iptables.nix ];
    options.tv.consul = api;
    config = mkIf cfg.enable (mkMerge [
      imp
      { tv.iptables.input-retiolum-accept-new-tcp = [ "8300" "8301" ]; }
      # TODO udp for 8301
    ]);
  };

  api = {
    # TODO inherit (lib) api.options.enable; oder so
    enable = mkOption {
      type = types.bool;
      default = false;
      description = "enable tv.consul";
    };
    dc = mkOption {
      type = types.unspecified;
    };
    hosts = mkOption {
      type = with types; listOf unspecified;
    };
    encrypt-file = mkOption {
      type = types.str; # TODO path (but not just into store)
      default = "/root/src/secrets/consul-encrypt.json";
    };
    data-dir = mkOption {
      type = types.str; # TODO path (but not just into store)
      default = "/var/lib/consul";
    };
    self = mkOption {
      type = types.unspecified;
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
    bind_addr = cfg.self.addr; # TODO cfg.addr
    enable_syslog = true;
    retry_join = map (getAttr "addr") (filter (host: host.fqdn != cfg.self.fqdn) cfg.hosts);
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
          chown consul: ${cfg.data-dir}
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

  user = {
    name = "consul";
    uid = 2983239726; # genid consul
  };

in
out
