{ config, pkgs, lib, ... }:

with import <stockholm/lib>;
let
  cfg = config.krebs.exim-smarthost;

  out = {
    options.krebs.exim-smarthost = api;
    config = lib.mkIf cfg.enable imp;
  };

  api = {
    enable = mkEnableOption "krebs.exim-smarthost";

    enableSPFVerification = mkEnableOption "SPF verification";

    authenticators = mkOption {
      type = types.attrsOf types.str;
      default = {};
    };

    dkim = mkOption {
      type = types.listOf (types.submodule ({ config, ... }: {
        options = {
          domain = mkOption {
            type = types.str;
          };
          private_key = mkOption {
            type = types.absolute-pathname;
            default = toString <secrets> + "/${config.domain}.dkim.priv";
            defaultText = "‹secrets/‹domain›.dkim.priv›";
          };
          selector = mkOption {
            type = types.str;
            default = "default";
          };
        };
      }));
      default = [];
    };

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
      default = unique (["localhost" cfg.primary_hostname] ++ config.krebs.build.host.nets.retiolum.aliases);
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

    ssl_cert = mkOption {
      type = types.nullOr types.str;
      default = null;
    };

    ssl_key = mkOption {
      type = types.nullOr types.str;
      default = null;
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
    krebs.systemd.services.exim = {};
    systemd.services.exim.serviceConfig.LoadCredential =
      map (dkim: "${dkim.domain}.dkim_private_key:${dkim.private_key}") cfg.dkim;
    krebs.exim = {
      enable = true;
      config = /* exim */ ''
        keep_environment = CREDENTIALS_DIRECTORY

        primary_hostname = ${cfg.primary_hostname}

        # HOST_REDIR contains the real destinations for "local_domains".
        #HOST_REDIR = /etc/exim4/host_redirect

        # Domains not listed in local_domains need to be deliverable remotely.
        # XXX We abuse local_domains to mean "domains, we're the gateway for".
        domainlist local_domains = ${concatStringsSep ":" cfg.local_domains}
        domainlist relay_to_domains = ${concatStringsSep ":" cfg.relay_to_domains}
        domainlist sender_domains = ${concatStringsSep ":" cfg.sender_domains}
        hostlist relay_from_hosts = <;${concatStringsSep ";" cfg.relay_from_hosts}

        acl_smtp_data = acl_check_data
        acl_smtp_mail = acl_check_mail
        acl_smtp_rcpt = acl_check_rcpt

        never_users = root

        host_lookup = *

        rfc1413_hosts = *
        rfc1413_query_timeout = 5s

        log_selector = -queue_run +address_rewrite +all_parents +queue_time
        log_file_path = syslog
        syslog_timestamp = false
        syslog_duplication = false

        ${optionalString (cfg.ssl_cert != null) "tls_certificate = ${cfg.ssl_cert}"}
        ${optionalString (cfg.ssl_key != null) "tls_privatekey = ${cfg.ssl_key}"}
        tls_advertise_hosts =${optionalString (cfg.ssl_cert != null) " *"}

        begin acl

        acl_check_rcpt:
          deny
            local_parts = ^[./|] : ^.*[@%!] : ^.*/\\.\\./
            message = restricted characters in address

          accept
            recipients = lsearch*@;${lsearch.internet-aliases}

          accept
            authenticated = *
            control = dkim_disable_verify
            control = submission

          accept
            control = dkim_disable_verify
            control = submission
            hosts = +relay_from_hosts

          accept
            domains = +local_domains : +relay_to_domains

          deny
            message = relay not permitted


        acl_check_data:
          warn
            sender_domains = +sender_domains
            set acl_m_special_dom = $sender_address_domain

          accept

        acl_check_mail:
          ${if cfg.enableSPFVerification then indent /* exim */ ''
            accept
              authenticated = *
            accept
              hosts = +relay_from_hosts
            deny
              spf = fail : softfail
              log_message = spf=$spf_result
              message = SPF validation failed: \
                      $sender_host_address is not allowed to send mail from \
                      ''${if def:sender_address_domain\
                             {$sender_address_domain}\
                             {$sender_helo_name}}
            deny
              spf = permerror
              log_message = spf=$spf_result
              message = SPF validation failed: \
                      syntax error in SPF record(s) for \
                      ''${if def:sender_address_domain\
                             {$sender_address_domain}\
                             {$sender_helo_name}}
            defer
              spf = temperror
              log_message = spf=$spf_result; deferred
              message = temporary error during SPF validation; \
                      please try again later
            warn
              spf = none : neutral
              log_message = spf=$spf_result
            accept
              add_header = $spf_received
          '' else indent /* exim */ ''
            accept
          ''}

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
          data = ''${lookup{$local_part@$domain}lsearch*@{${lsearch.internet-aliases}}}

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
          ${optionalString (cfg.dkim != []) (indent /* exim */ ''
            dkim_canon = relaxed
            dkim_domain = $sender_address_domain
            dkim_private_key = ''${lookup{$sender_address_domain.dkim_private_key}dsearch,ret=full{''${env{CREDENTIALS_DIRECTORY}{$value}fail}}}
            dkim_selector = ''${lookup{$sender_address_domain}lsearch{${lsearch.dkim_selector}}}
            dkim_strict = true
          '')}
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
        ${concatStringsSep "\n" (mapAttrsToList (name: text: /* exim */ ''
        ${name}:
          ${indent text}
        '') cfg.authenticators)}
      '';
    };
  };


  lsearch = mapAttrs (name: set: toFile name (to-lsearch set)) ({
    inherit (cfg) internet-aliases;
    inherit (cfg) system-aliases;
  } // optionalAttrs (cfg.dkim != []) {
    dkim_selector = flip map cfg.dkim (dkim: {
      from = dkim.domain;
      to = dkim.selector;
    });
  });

  to-lsearch = concatMapStrings ({ from, to, ... }: "${from}: ${to}\n");

in out
