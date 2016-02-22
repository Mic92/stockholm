{ config, lib, pkgs, ... }:

with config.krebs.lib;
let
  cfg = config.tv.iptables;

  out = {
    options.tv.iptables = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "tv.iptables";

    accept-echo-request = mkOption {
      type = with types; nullOr (enum ["internet" "retiolum"]);
      default = "retiolum";
    };

    input-internet-accept-new-tcp = mkOption {
      type = with types; listOf (either int str);
      default = [];
    };

    input-retiolum-accept-new-tcp = mkOption {
      type = with types; listOf (either int str);
      default = [];
    };

    extra = {
      nat.POSTROUTING = mkOption {
        type = with types; listOf str;
        default = [];
      };
      filter.FORWARD = mkOption {
        type = with types; listOf str;
        default = [];
      };
      filter.INPUT = mkOption {
        type = with types; listOf str;
        default = [];
      };
    };
  };

  imp = {
    networking.firewall.enable = false;

    systemd.services.tv-iptables = {
      description = "tv-iptables";
      wantedBy = [ "network-pre.target" ];
      before = [ "network-pre.target" ];
      after = [ "systemd-modules-load.service" ];

      path = with pkgs; [
        iptables
      ];

      restartIfChanged = true;

      serviceConfig = {
        Type = "simple";
        RemainAfterExit = true;
        Restart = "always";
        SyslogIdentifier = "tv-iptables_start";
        ExecStart = pkgs.writeDash "tv-iptables_start" ''
          set -euf
          iptables-restore < ${rules 4}
          ip6tables-restore < ${rules 6}
        '';
      };
    };
  };

  formatTable = table:
    (concatStringsSep "\n"
      (mapAttrsToList
        (chain: concatMapStringsSep "\n" (rule: "-A ${chain} ${rule}"))
        table));

  rules = iptables-version: let
    accept-echo-request = {
      ip4tables = "-p icmp -m icmp --icmp-type echo-request -j ACCEPT";
      ip6tables = "-p ipv6-icmp -m icmp6 --icmpv6-type echo-request -j ACCEPT";
    }."ip${toString iptables-version}tables";
    accept-new-tcp = port:
      "-p tcp -m tcp --dport ${port} -m conntrack --ctstate NEW -j ACCEPT";
  in
    pkgs.writeText "tv-iptables-rules${toString iptables-version}" ''
      *nat
      :PREROUTING ACCEPT [0:0]
      :INPUT ACCEPT [0:0]
      :OUTPUT ACCEPT [0:0]
      :POSTROUTING ACCEPT [0:0]
      ${concatMapStringsSep "\n" (rule: "-A PREROUTING ${rule}") [
        "! -i retiolum -p tcp -m tcp --dport 22 -j REDIRECT --to-ports 0"
        "-p tcp -m tcp --dport 11423 -j REDIRECT --to-ports 22"
      ]}
      ${concatMapStringsSep "\n" (rule: "-A OUTPUT ${rule}") [
        "-o lo -p tcp -m tcp --dport 11423 -j REDIRECT --to-ports 22"
      ]}
      ${formatTable cfg.extra.nat}
      COMMIT
      *filter
      :INPUT DROP [0:0]
      :FORWARD DROP [0:0]
      :OUTPUT ACCEPT [0:0]
      :Retiolum - [0:0]
      ${concatMapStringsSep "\n" (rule: "-A INPUT ${rule}") ([]
        ++ [
          "-m conntrack --ctstate RELATED,ESTABLISHED -j ACCEPT"
          "-i lo -j ACCEPT"
        ]
        ++ optional (cfg.accept-echo-request == "internet") accept-echo-request
        ++ map accept-new-tcp (unique (map toString cfg.input-internet-accept-new-tcp))
        ++ ["-i retiolum -j Retiolum"]
      )}
      ${formatTable cfg.extra.filter}
      ${concatMapStringsSep "\n" (rule: "-A Retiolum ${rule}") ([]
        ++ optional (cfg.accept-echo-request == "retiolum") accept-echo-request
        ++ map accept-new-tcp (unique (map toString cfg.input-retiolum-accept-new-tcp))
        ++ {
          ip4tables = [
            "-p tcp -j REJECT --reject-with tcp-reset"
            "-p udp -j REJECT --reject-with icmp-port-unreachable"
            "-j REJECT --reject-with icmp-proto-unreachable"
          ];
          ip6tables = [
            "-p tcp -j REJECT --reject-with tcp-reset"
            "-p udp -j REJECT --reject-with icmp6-port-unreachable"
            "-j REJECT"
          ];
        }."ip${toString iptables-version}tables"
      )}
      COMMIT
    '';
in out

#let
#  cfg = config.tv.iptables;
#  arg' = arg // { inherit cfg; };
#in
#
#{
#  options.tv.iptables = import ./options.nix arg';
#  config = lib.mkIf cfg.enable (import ./config.nix arg');
#}
