{ config, pkgs, lib, ... }:
with import ../../lib/pure.nix { inherit lib; }; let
  cfg = config.krebs.exim-retiolum;

  # Due to improvements to the JSON notation, braces around top-level objects
  # are not necessary^Wsupported by rspamd's parser when including files:
  # https://github.com/rspamd/rspamd/issues/2674
  toMostlyJSON = value:
    assert typeOf value == "set";
    (s: substring 1 (stringLength s - 2) s)
    (toJSON value);

  to-lsearch = concatMapStrings ({ from, to, ... }: "${from}: ${to}\n");
  lsearch = mapAttrs (name: set: toFile name (to-lsearch set)) ({
    inherit (cfg) system-aliases;
  });

in {
  options.krebs.exim-retiolum = {
    enable = mkEnableOption "krebs.exim-retiolum";
    local_domains = mkOption {
      type = with types; listOf hostname;
      default = ["localhost"] ++ config.krebs.build.host.nets.retiolum.aliases;
    };
    primary_hostname = mkOption {
      type = types.str;
      default = let x = "${config.krebs.build.host.name}.r"; in
        assert elem x config.krebs.build.host.nets.retiolum.aliases;
        x;
    };
    relay_to_domains = mkOption {
      # TODO hostname with wildcards
      type = with types; listOf str;
      default = [
        "*.r"
      ];
    };
    rspamd = {
      enable = mkEnableOption "krebs.exim-retiolum.rspamd" // {
        default = false;
      };
      locals = {
        logging = {
          level = mkOption {
            type = types.enum [
              "error"
              "warning"
              "notice"
              "info"
              "debug"
              "silent"
            ];
            default = "notice";
          };
        };
        options = {
          local_networks = mkOption {
            type = types.listOf types.cidr;
            default = [
              config.krebs.build.host.nets.retiolum.ip4.prefix
              config.krebs.build.host.nets.retiolum.ip6.prefix
            ];
          };
        };
      };
    };
    system-aliases = mkOption {
      type = types.listOf (types.submodule ({
        options = {
          from = mkOption {
            type = types.str; # TODO e-mail address
          };
          to = mkOption {
            type = types.str; # TODO e-mail address / TODO listOf
          };
        };
      }));
      default = [];
    };
  };
  imports = [
    {
      config = lib.mkIf cfg.rspamd.enable {
        services.rspamd.enable = true;
        services.rspamd.locals =
          mapAttrs'
            (name: value: nameValuePair "${name}.inc" {
              text = toMostlyJSON value;
            })
            cfg.rspamd.locals;
        users.users.${config.krebs.exim.user.name}.extraGroups = [
          config.services.rspamd.group
        ];
      };
    }
  ];
  config = lib.mkIf cfg.enable {
    krebs.exim = {
      enable = true;
      config =
        # This configuration makes only sense for retiolum-enabled hosts.
        # TODO modular configuration
        assert config.krebs.tinc.retiolum.enable;
        /* exim */ ''
          keep_environment =

          primary_hostname = ${cfg.primary_hostname}
          domainlist local_domains = ${concatStringsSep ":" cfg.local_domains}
          domainlist relay_to_domains = ${concatStringsSep ":" cfg.relay_to_domains}

          ${optionalString cfg.rspamd.enable /* exim */ ''
            spamd_address = /run/rspamd/rspamd.sock variant=rspamd
          ''}

          acl_smtp_rcpt = acl_check_rcpt
          acl_smtp_data = acl_check_data

          host_lookup = *
          rfc1413_hosts = *
          rfc1413_query_timeout = 5s

          log_file_path = syslog
          syslog_timestamp = false
          syslog_duplication = false

          tls_advertise_hosts =

          begin acl

          acl_check_rcpt:
            deny
              local_parts = ^[./|] : ^.*[@%!] : ^.*/\\.\\./
              message = restricted characters in address

            accept
              domains = +local_domains : +relay_to_domains

            deny
              message = relay not permitted


          acl_check_data:
            ${optionalString cfg.rspamd.enable /* exim */ ''
              accept condition = ''${if eq{$interface_port}{587}}

              warn remove_header = ${concatStringsSep " : " [
                "x-spam"
                "x-spam-report"
                "x-spam-score"
              ]}

              warn
                spam = nobody:true

              warn
                condition = ''${if !eq{$spam_action}{no action}}
                add_header = X-Spam: Yes
                add_header = X-Spam-Report: $spam_report
                add_header = X-Spam-Score: $spam_score
            ''}
            accept


          begin routers

          system_aliases:
            debug_print = "R: system_aliases for $local_part@$domain"
            driver = redirect
            data = ''${lookup{$local_part}lsearch{${lsearch.system-aliases}}}

          local:
            driver = accept
            domains = +local_domains
            check_local_user
          # local_part_suffix = +*
          # local_part_suffix_optional
            transport = home_maildir

          remote:
            driver = manualroute
            domains = +relay_to_domains
            transport = remote_smtp
            route_list = ^.* $0 byname


          begin transports

          remote_smtp:
            driver = smtp

          home_maildir:
            driver = appendfile
            maildir_format
            directory = $home/Maildir
            directory_mode = 0700
            delivery_date_add
            envelope_to_add
            return_path_add
          # group = mail
          # mode = 0660

          begin retry
          ${concatMapStringsSep "\n" (k: "${k} * F,42d,1m") cfg.relay_to_domains}
          * * F,2h,15m; G,16h,1h,1.5; F,4d,6h

          begin rewrite

          begin authenticators
        '';
    };
  };
}
