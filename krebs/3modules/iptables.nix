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
          #TODO: find out good defaults.
          policy = mkOption {
            type = str;
            default = "ACCEPT";
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
                v4 = mkOption {
                  type = bool;
                  default = true;
                };
                v6 = mkOption {
                  type = bool;
                  default = true;
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
        ExecStart = startScript;
      };

      unitConfig.DefaultDependencies = false;
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
          filteredRules = filter (r: r."${v}") ts."${tn}"."${cn}".rules;
          sortedRules = sort (a: b: a.precedence > b.precedence) filteredRules;

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
    pkgs.writeText "krebs-iptables-rules${iptables-version}" ''
      ${buildTables iptables-version cfg.tables}
    '';

  startScript = pkgs.writeDash "krebs-iptables_start" ''
    set -euf
    iptables-restore < ${rules "v4"}
    ip6tables-restore < ${rules "v6"}
  '';

in
out

