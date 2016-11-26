{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let
  inherit (pkgs) writeText;

  inherit (builtins)
    elem
  ;

  cfg = config.krebs.iptables;

  out = {
    options.krebs.iptables = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "iptables";

    #tables.filter.INPUT = {
    # policy = "DROP";
    # rules = [
    #   { predicate = "-i retiolum"; target = "ACCEPT"; priority = -10; }
    # ];
    #};
    #new api
    tables = mkOption {
      type = with types; attrsOf (attrsOf (submodule ({
        options = {
          policy = mkOption {
            type = str;
            default = "-";
          };
          rules = mkOption {
            type = nullOr (listOf (submodule ({
              options = {
                predicate = mkOption {
                  type = str;
                };
                target = mkOption {
                  type = str;
                };
                precedence = mkOption {
                  type = int;
                  default = 0;
                };
              };
            })));
            default = null;
          };
        };
      })));
    };
  };

  imp = {
    networking.firewall.enable = false;

    systemd.services.krebs-iptables = {
      description = "krebs-iptables";
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
        ExecStart = startScript;
      };
    };
  };

  #buildTable :: iptablesVersion -> iptablesAttrSet` -> str
  #todo: differentiate by iptables-version
  buildTables = v: ts:
    let

      declareChain = t: cn:
        #TODO: find out what to do whit these count numbers
        ":${cn} ${t."${cn}".policy} [0:0]";

      buildChain = tn: cn:
        let
          sortedRules = sort (a: b: a.precedence > b.precedence) ts."${tn}"."${cn}".rules;

        in
          #TODO: double check should be unneccessary, refactor!
          if ts.${tn}.${cn}.rules or null != null then
            concatMapStringsSep "\n" (rule: "\n-A ${cn} ${rule}") ([]
              ++ map (buildRule tn cn) sortedRules
            )
          else
            ""
          ;


      buildRule = tn: cn: rule:
        "${rule.predicate} -j ${rule.target}";

      buildTable = tn:
        "*${tn}\n" +
        concatStringsSep "\n" ([]
          ++ map (declareChain ts."${tn}") (attrNames ts."${tn}")
        ) +
        #this looks dirty, find a better way to do this (maybe optionalString)
        concatStringsSep "" ([]
          ++ map (buildChain tn) (attrNames ts."${tn}")
        ) +
        "\nCOMMIT";
    in
      concatStringsSep "\n" ([]
        ++ map buildTable (attrNames ts)
      );

#=====

  rules = iptables-version:
    let
      #TODO: find out good defaults.
      tables-defaults = {
        nat.PREROUTING.policy = "ACCEPT";
        nat.INPUT.policy = "ACCEPT";
        nat.OUTPUT.policy = "ACCEPT";
        nat.POSTROUTING.policy = "ACCEPT";
        filter.INPUT.policy = "ACCEPT";
        filter.FORWARD.policy = "ACCEPT";
        filter.OUTPUT.policy = "ACCEPT";

        #if someone specifies any other rules on this chain, the default rules get lost.
        #is this wanted beahiviour or a bug?
        #TODO: implement abstraction of rules
        filter.INPUT.rules = [
          { predicate = "-m conntrack --ctstate RELATED,ESTABLISHED"; target = "ACCEPT"; }
        ];
      };
      tables = tables-defaults // cfg.tables;

    in
      pkgs.writeText "krebs-iptables-rules${iptables-version}" ''
        ${buildTables iptables-version tables}
      '';

  startScript = pkgs.writeDash "krebs-iptables_start" ''
    set -euf
    iptables-restore < ${rules "v4"}
    ip6tables-restore < ${rules "v6"}
  '';

in
out

