{ config, lib, pkgs, ... }: with import ../../lib/pure.nix { inherit lib; }; let
  out = {
    options.krebs.on-failure = api;
    config = lib.mkIf cfg.enable imp;
  };

  cfg = config.krebs.on-failure;

  api = {
    enable = mkEnableOption "krebs.on-failure" // {
      default = cfg.plans != {};
    };
    plans = mkOption {
      default = {};
      type = let
        inherit (config) krebs;
      in types.attrsOf (types.submodule ({ config, ... }: {
        options = {
          enable = mkEnableOption "krebs.on-failure.${config.name}" // {
            default = true;
          };
          journalctl = {
            lines = mkOption {
              type = types.int;
              default = 100;
            };
            output = mkOption {
              type = types.enum [
                "cat"
                "export"
                "json"
                "json-pretty"
                "json-sse"
                "short"
                "short-iso"
                "short-precise"
                "verbose"
              ];
              default = "short-iso";
            };
          };
          mailto = mkOption {
            type = types.str;
            default = krebs.build.user.mail;
            description = "Mail address to send journal extract to.";
          };
          subject = mkOption {
            type = types.str;
            default = "[${krebs.build.host.name}] ${config.name} has failed";
          };
          name = mkOption {
            type = types.str;
            default = config._module.args.name;
            description = "Name of the to-be-monitored service.";
          };
        };
      }));
    };
    sendmail = mkOption {
      type = types.str;
      default = "/run/wrappers/bin/sendmail";
    };
  };

  imp = {
    systemd.services = foldl (a: b: a // b) {} (map to-services enabled-plans);
  };

  enabled-plans = filter (getAttr "enable") (attrValues cfg.plans);

  to-services = plan: {
    "${plan.name}".unitConfig.OnFailure = "on-failure.${plan.name}.service";
    "on-failure.${plan.name}".serviceConfig = rec {
      ExecStart = start plan;
      SyslogIdentifier = ExecStart.name;
      Type = "oneshot";
    };
  };

  start = plan: pkgs.writeDash "on-failure.${plan.name}" ''
    { echo Subject: ${shell.escape plan.subject}
      echo To: ${shell.escape plan.mailto}
      echo
      ${pkgs.systemd}/bin/journalctl \
          --lines=${toString plan.journalctl.lines} \
          --output=${plan.journalctl.output} \
          --since="$(
            ${pkgs.coreutils}/bin/date +'%F %T UTC' -ud "$(
              ${pkgs.systemd}/bin/systemctl show \
                  -p ExecMainStartTimestamp \
                  ${shell.escape plan.name} \
                | ${pkgs.coreutils}/bin/cut -d= -f2-
            )"
          )" \
          --unit=${shell.escape plan.name}.service
    } | ${shell.escape cfg.sendmail} -t
  '';

in out
