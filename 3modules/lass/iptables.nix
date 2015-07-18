arg@{ config, lib, pkgs, ... }:

let
  inherit (pkgs) writeScript writeText;

  inherit (lib)
    concatMapStringsSep
    concatStringsSep
    attrNames
    unique
    fold
    any
    attrValues
    catAttrs
    filter
    flatten
    length
    hasAttr
    mkEnableOption
    mkOption
    mkIf
    types
    sort;

  elemIsIn = a: as:
    any (x: x == a) as;

  cfg = config.lass.iptables;

  out = {
    options.lass.iptables = api;
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

    systemd.services.lass-iptables = {
      description = "lass-iptables";
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
        ExecStart = "@${startScript} lass-iptables_start";
      };
    };
  };

  #buildTable :: iptablesVersion -> iptablesAttrSet` -> str
  #todo: differentiate by iptables-version
  buildTables = v: ts:
    let
      sortedTable = sort (a: b: a.precedence < b.precedence) ts;

      declareChain = t: cn:
        #TODO: find out what to do whit these count numbers
        ":${cn} ${t."${cn}".policy} [0:0]";

      buildChain = tn: cn:
      #"${concatStringsSep " " ((attrNames t."${cn}") ++ [cn])}";

      #TODO: double check should be unneccessary, refactor!
        if (hasAttr "rules" ts."${tn}"."${cn}") then
          if (ts."${tn}"."${cn}".rules == null) then
            ""
          else
            concatMapStringsSep "\n" (rule: "\n-A ${cn} ${rule}") ([]
              ++ map (buildRule tn cn) ts."${tn}"."${cn}".rules
            )
        else
          ""
        ;


      buildRule = tn: cn: rule:
        #target validation test:
        assert (elemIsIn rule.target ([ "ACCEPT" "REJECT" "DROP" "QUEUE" "LOG" "RETURN" ] ++ ts."${tn}"."${cn}"));

        #predicate validation test:
        #maybe use iptables-test
        #TODO: howto exit with evaluation error by shellscript?
          #apperantly not possible from nix because evalatution wouldn't be deterministic.
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
        ++ map buildTable (attrNames sortedTable)
      );

#=====

  rules4 = iptables-version:
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
      writeText "lass-iptables-rules${toString iptables-version}" ''
        ${buildTables iptables-version tables}
      '';

  startScript = writeScript "lass-iptables_start" ''
    #! /bin/sh
    set -euf
    iptables-restore < ${rules4 4}
    ip6tables-restore < ${rules4 6}
  '';

in
out

