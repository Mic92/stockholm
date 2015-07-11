{ config, pkgs, ... }:

{
  services.exim =
    # This configuration makes only sense for retiolum-enabled hosts.
    # TODO modular configuration
    assert config.tv.retiolum.enable;
    let
      # TODO get the hostname from config.tv.retiolum.
      retiolumHostname = "${config.networking.hostName}.retiolum";
    in
      { enable = true;
        config = ''
          primary_hostname = ${retiolumHostname}
          domainlist local_domains    = @ : localhost
          domainlist relay_to_domains = *.retiolum
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
            domains = ! ${retiolumHostname} : *.retiolum
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
          *.retiolum             *           F,42d,1m
          *                      *           F,2h,15m; G,16h,1h,1.5; F,4d,6h

          begin rewrite

          begin authenticators
        '';
      };
}
