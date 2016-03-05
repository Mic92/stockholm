{ config, pkgs, lib, ... }:

with config.krebs.lib;
let
  cfg = config.krebs.exim-retiolum;

  out = {
    options.krebs.exim-retiolum = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
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
        "*.retiolum"
      ];
    };
  };

  imp = {
    services.exim = {
      enable = true;
      config =
        # This configuration makes only sense for retiolum-enabled hosts.
        # TODO modular configuration
        assert config.krebs.retiolum.enable;
        ''
          keep_environment =

          primary_hostname = ${cfg.primary_hostname}
          domainlist local_domains = ${concatStringsSep ":" cfg.local_domains}
          domainlist relay_to_domains = ${concatStringsSep ":" cfg.relay_to_domains}
          hostlist   relay_from_hosts = <; 127.0.0.1 ; ::1

          acl_smtp_rcpt = acl_check_rcpt
          acl_smtp_data = acl_check_data

          host_lookup = *
          rfc1413_hosts = *
          rfc1413_query_timeout = 5s

          log_file_path = syslog
          syslog_timestamp = false
          syslog_duplication = false

          begin acl

          acl_check_rcpt:
            accept  hosts = :
                    control = dkim_disable_verify

            deny    message       = Restricted characters in address
                    domains       = +local_domains
                    local_parts   = ^[.] : ^.*[@%!/|]

            deny    message       = Restricted characters in address
                    domains       = !+local_domains
                    local_parts   = ^[./|] : ^.*[@%!] : ^.*/\\.\\./

            accept  local_parts   = postmaster
                    domains       = +local_domains

            #accept
            #  hosts = *.retiolum
            #  domains = *.retiolum
            #  control = dkim_disable_verify

            #require verify        = sender

            accept  hosts         = +relay_from_hosts
                    control       = submission
                    control       = dkim_disable_verify

            accept  authenticated = *
                    control       = submission
                    control       = dkim_disable_verify

            require message = relay not permitted
                    domains = +local_domains : +relay_to_domains

            require verify = recipient

            accept


          acl_check_data:
            accept


          begin routers

          retiolum:
            driver = manualroute
            domains = ! +local_domains : +relay_to_domains
            transport = remote_smtp
            route_list = ^.* $0 byname
            no_more

          nonlocal:
            debug_print = "R: nonlocal for $local_part@$domain"
            driver = redirect
            domains = ! +local_domains
            allow_fail
            data = :fail: Mailing to remote domains not supported
            no_more

          local_user:
            # debug_print = "R: local_user for $local_part@$domain"
            driver = accept
            check_local_user
          # local_part_suffix = +* : -*
          # local_part_suffix_optional
            transport = home_maildir
            cannot_route_message = Unknown user


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
in out
