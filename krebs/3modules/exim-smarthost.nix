{ config, pkgs, lib, ... }:

with config.krebs.lib;
let
  cfg = config.krebs.exim-smarthost;

  out = {
    options.krebs.exim-smarthost = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs.exim-smarthost";

    internet-aliases = mkOption {
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
    };

    local_domains = mkOption {
      type = with types; listOf hostname;
      default = ["localhost"] ++ config.krebs.build.host.nets.retiolum.aliases;
    };

    relay_from_hosts = mkOption {
      type = with types; listOf str;
      default = [];
      apply = xs: ["127.0.0.1" "::1"] ++ xs;
    };

    relay_to_domains = mkOption {
      # TODO hostname with wildcards
      type = with types; listOf str;
      default = [
        "*.r"
        "*.retiolum"
      ];
    };

    primary_hostname = mkOption {
      type = types.str;
      default = let x = "${config.krebs.build.host.name}.r"; in
        assert elem x config.krebs.build.host.nets.retiolum.aliases;
        x;
    };

    sender_domains = mkOption {
      type = with types; listOf str;
      default = [];
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
    };
  };

  imp = {
    services.exim = {
      enable = true;
      config = ''
        keep_environment =

        primary_hostname = ${cfg.primary_hostname}

        # HOST_REDIR contains the real destinations for "local_domains".
        #HOST_REDIR = /etc/exim4/host_redirect

        # Domains not listed in local_domains need to be deliverable remotely.
        # XXX We abuse local_domains to mean "domains, we're the gateway for".
        domainlist local_domains = ${concatStringsSep ":" cfg.local_domains}
        domainlist relay_to_domains = ${concatStringsSep ":" cfg.relay_to_domains}
        hostlist relay_from_hosts = <;${concatStringsSep ";" cfg.relay_from_hosts}

        acl_smtp_rcpt = acl_check_rcpt
        acl_smtp_data = acl_check_data

        never_users = root

        host_lookup = *

        rfc1413_hosts = *
        rfc1413_query_timeout = 5s

        log_selector = -queue_run +address_rewrite +all_parents +queue_time
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

          accept  hosts         = +relay_from_hosts
                  control       = submission
                  control       = dkim_disable_verify

          accept  authenticated = *
                  control       = submission
                  control       = dkim_disable_verify

          accept message = relay not permitted 2
                  recipients = lsearch;${lsearch.internet-aliases}

          require message = relay not permitted
                  domains = +local_domains : +relay_to_domains

          require
            message = unknown user
            verify = recipient/callout

          accept


        acl_check_data:
          warn
            sender_domains = ${concatStringsSep ":" cfg.sender_domains}
            set acl_m_special_dom = $sender_address_domain

          accept


        begin routers

        # feature RETIOLUM_MAIL
        retiolum:
          debug_print = "R: retiolum for $local_part@$domain"
          driver = manualroute
          domains = ! +local_domains : +relay_to_domains
          transport = retiolum_smtp
          route_list = ^.* $0 byname
          no_more

        internet_aliases:
          debug_print = "R: internet_aliases for $local_part@$domain"
          driver = redirect
          data = ''${lookup{$local_part@$domain}lsearch{${lsearch.internet-aliases}}}

        dnslookup:
          debug_print = "R: dnslookup for $local_part@$domain"
          driver = dnslookup
          domains = ! +local_domains
          transport = remote_smtp
          ignore_target_hosts = 0.0.0.0 : 127.0.0.0/8
          no_more

        system_aliases:
          debug_print = "R: system_aliases for $local_part@$domain"
          driver = redirect
          data = ''${lookup{$local_part}lsearch{${lsearch.system-aliases}}}

        local_user:
          debug_print = "R: local_user for $local_part@$domain"
          driver = accept
          check_local_user
          transport = home_maildir
          cannot_route_message = Unknown user

        begin transports

        retiolum_smtp:
          driver = smtp
          retry_include_ip_address = false

        remote_smtp:
          driver = smtp
          helo_data = ''${if eq{$acl_m_special_dom}{}  \
                               {$primary_hostname}   \
                               {$acl_m_special_dom} }

        home_maildir:
          driver = appendfile
          maildir_format
          maildir_use_size_file
          directory = $home/Mail
          directory_mode = 0700
          delivery_date_add
          envelope_to_add
          return_path_add

        begin retry
        ${concatMapStringsSep "\n" (k: "${k} * F,42d,1m") cfg.relay_to_domains}
        ${concatMapStringsSep "\n" (k: "${k} * F,42d,1m")
                                   # TODO don't include relay_to_domains
                                   (map (getAttr "from") cfg.internet-aliases)}
        * * F,2h,15m; G,16h,1h,1.5; F,4d,6h

        begin rewrite
        begin authenticators
      '';
    };
  };


  lsearch = mapAttrs (name: set: toFile name (to-lsearch set)) {
    inherit (cfg) internet-aliases;
    inherit (cfg) system-aliases;
  };

  to-lsearch = concatMapStringsSep "\n" ({ from, to, ... }: "${from}: ${to}");

in
out
