with import ./lib;
{ config, pkgs, ... }: let {
  cfg = config.tv.iptables;

  body = {
    options.tv.iptables = api;
    config = lib.mkIf cfg.enable imp;
  };

  extraTypes = {
    rules = types.submodule {
      options = {
        nat.OUTPUT = mkOption {
          type = with types; listOf str;
          default = [];
        };
        nat.PREROUTING = mkOption {
          type = with types; listOf str;
          default = [];
        };
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
        filter.Retiolum = mkOption {
          type = with types; listOf str;
          default = [];
        };
      };
    };
  };

  api = {
    enable = mkEnableOption "tv.iptables";

    accept-echo-request = mkOption {
      type = with types; nullOr (enum ["internet" "retiolum"]);
      default = "retiolum";
    };

    input-internet-accept-tcp = mkOption {
      type = with types; listOf (either int str);
      default = [];
    };

    input-internet-accept-udp = mkOption {
      type = with types; listOf (either int str);
      default = [];
    };

    input-retiolum-accept-tcp = mkOption {
      type = with types; listOf (either int str);
      default = [];
    };

    input-retiolum-accept-udp = mkOption {
      type = with types; listOf (either int str);
      default = [];
    };

    extra = mkOption {
      default = {};
      type = extraTypes.rules;
    };

    extra4 = mkOption {
      default = {};
      type = extraTypes.rules;
    };

    extra6 = mkOption {
      default = {};
      type = extraTypes.rules;
    };
  };

  imp = {
    networking.firewall.enable = false;

    systemd.services.tv-iptables = {
      wantedBy = [ "sysinit.target" ];
      wants = [ "network-pre.target" ];
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

      unitConfig.DefaultDependencies = false;
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
    accept-tcp = port: "-p tcp -m tcp --dport ${port} -j ACCEPT";
    accept-udp = port: "-p udp -m udp --dport ${port} -j ACCEPT";
  in
    pkgs.writeText "tv-iptables-rules${toString iptables-version}" ''
      *nat
      :PREROUTING ACCEPT [0:0]
      :INPUT ACCEPT [0:0]
      :OUTPUT ACCEPT [0:0]
      :POSTROUTING ACCEPT [0:0]
      ${formatTable cfg."extra${toString iptables-version}".nat}
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
        ++ map accept-tcp (unique (map toString cfg.input-internet-accept-tcp))
        ++ map accept-udp (unique (map toString cfg.input-internet-accept-udp))
        ++ ["-i retiolum -j Retiolum"]
      )}
      ${formatTable cfg.extra.filter}
      ${formatTable cfg."extra${toString iptables-version}".filter}
      ${concatMapStringsSep "\n" (rule: "-A Retiolum ${rule}") ([]
        ++ optional (cfg.accept-echo-request == "retiolum") accept-echo-request
        ++ map accept-tcp (unique (map toString cfg.input-retiolum-accept-tcp))
        ++ map accept-udp (unique (map toString cfg.input-retiolum-accept-udp))
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
}
