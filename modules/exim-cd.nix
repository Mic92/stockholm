{ config, pkgs, ... }:

let
  inherit (builtins) toFile;
  inherit (pkgs.lib.attrsets) mapAttrs;
  inherit (pkgs.lib.strings) concatMapStringsSep;
in

{
  services.exim =
    let
      retiolumHostname = "${config.networking.hostName}.retiolum";

      internet-aliases = [
        { from = "tomislav@viljetic.de"; to = "tv@wu.retiolum"; }

        # (mindestens) lisp-stammtisch und elli haben die:
        { from = "tv@viljetic.de"; to = "tv@wu.retiolum"; }

        { from = "tv@destroy.dyn.shackspace.de"; to = "tv@wu.retiolum"; }

        { from = "mirko@viljetic.de"; to = "mv@cd.retiolum"; }

        # TODO killme (wo wird die benutzt?)
        { from = "tv@cd.retiolum"; to = "tv@wu.retiolum"; }

        { from = "postmaster@krebsco.de"; to = "tv@wu.retiolum"; }
      ];

      system-aliases = [
        { from = "mailer-daemon"; to = "postmaster"; }
        { from = "postmaster"; to = "root"; }
        { from = "nobody"; to = "root"; }
        { from = "hostmaster"; to = "root"; }
        { from = "usenet"; to = "root"; }
        { from = "news"; to = "root"; }
        { from = "webmaster"; to = "root"; }
        { from = "www"; to = "root"; }
        { from = "ftp"; to = "root"; }
        { from = "abuse"; to = "root"; }
        { from = "noc"; to = "root"; }
        { from = "security"; to = "root"; }
        { from = "root"; to = "tv"; }
      ];

      to-lsearch = concatMapStringsSep "\n" ({ from, to }: "${from}: ${to}");
      lsearch =
        mapAttrs (name: set: toFile name (to-lsearch set)) {
          inherit internet-aliases;
          inherit system-aliases;
        };
    in
    {
      enable = true;
      config =
        ''
          primary_hostname = ${retiolumHostname}

          # HOST_REDIR contains the real destinations for "local_domains".
          #HOST_REDIR = /etc/exim4/host_redirect


          # Domains not listed in local_domains need to be deliverable remotely.
          # XXX We abuse local_domains to mean "domains, we're the gateway for".
          domainlist local_domains    = @ : localhost
          #: viljetic.de : SHACK_REDIR_HOSTNAME
          domainlist relay_to_domains =
          hostlist   relay_from_hosts = <; 127.0.0.1 ; ::1 ; 10.243.13.37

          acl_smtp_rcpt = acl_check_rcpt
          acl_smtp_data = acl_check_data

          # av_scanner = clamd:/tmp/clamd
          # spamd_address = 127.0.0.1 783

          # tls_advertise_hosts = *
          # tls_certificate = /etc/ssl/exim.crt
          # tls_privatekey = /etc/ssl/exim.pem
          # (debian) tls_verify_certificates (to check client certs)

          # daemon_smtp_ports = 25 : 465 : 587
          # tls_on_connect_ports = 465

          # qualify_domain defaults to primary_hostname
          # qualify_recipient defaults to qualify_domain

          # allow_domain_literals

          never_users = root

          host_lookup = *

          # ident callbacks for all incoming SMTP calls
          rfc1413_hosts = *
          rfc1413_query_timeout = 5s

          # sender_unqualified_hosts =
          # recipient_unqualified_hosts =

          # percent_hack_domains =

          # arch & debian
          #ignore_bounce_errors_after = 2d
          #timeout_frozen_after = 7d
          # debian
          #smtp_banner = $smtp_active_hostname ESMTP Exim $version_number $tod_full
          #freeze_tell = postmaster
          #trusted_users = uucp
          # arch
          #split_spool_directory = true

          log_selector = -queue_run +address_rewrite +all_parents +queue_time
          log_file_path = syslog
          syslog_timestamp = false
          syslog_duplication = false

          begin acl

          acl_check_rcpt:
            # Accept if the source is local SMTP (i.e. not over TCP/IP).
            # We do this by testing for an empty sending host field.
            accept  hosts = :
                    # arch & debian:
                    control = dkim_disable_verify

            deny    message       = Restricted characters in address
                    domains       = +local_domains
                    local_parts   = ^[.] : ^.*[@%!/|]

            deny    message       = Restricted characters in address
                    domains       = !+local_domains
                    local_parts   = ^[./|] : ^.*[@%!] : ^.*/\\.\\./

            accept  local_parts   = postmaster
                    domains       = +local_domains

            ## feature RETIOLUM_MAIL
            #accept
            #  hosts = *.retiolum
            #  domains = *.retiolum
            #  control = dkim_disable_verify

            #require verify        = sender

            accept  hosts         = +relay_from_hosts
                    control       = submission
                    # debian: control = submission/sender_retain
                    # arch & debian:
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

            # deny    message       = rejected because $sender_host_address is in a black list at $dnslist_domain\n$dnslist_text
            #         dnslists      = black.list.example
            #
            # warn    dnslists      = black.list.example
            #         add_header    = X-Warning: $sender_host_address is in a black list at $dnslist_domain
            #         log_message   = found in $dnslist_domain

            # Client SMTP Authorization (csa) checks on the sending host.
            # Such checks do DNS lookups for special SRV records.
            # require verify = csa

            accept


          acl_check_data:
            # see av_scanner
            #deny    malware    = *
            #        message    = This message contains a virus ($malware_name).

            # Add headers to a message if it is judged to be spam. Before enabling this,
            # you must install SpamAssassin. You may also need to set the spamd_address
            # option above.
            #
            # warn    spam       = nobody
            #         add_header = X-Spam_score: $spam_score\n\
            #                      X-Spam_score_int: $spam_score_int\n\
            #                      X-Spam_bar: $spam_bar\n\
            #                      X-Spam_report: $spam_report

            # feature HELO_REWRITE
            # XXX note that the public ip (162.219.5.183) resolves to viljetic.de
            warn
              sender_domains = viljetic.de : shackspace.de
              set acl_m_special_dom = $sender_address_domain

            accept


          begin routers

          # feature RETIOLUM_MAIL
          retiolum:
            debug_print = "R: retiolum for $local_part@$domain"
            driver = manualroute
            domains = ! ${retiolumHostname} : *.retiolum
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
            # if ipv6-enabled then instead use:
            # ignore_target_hosts = <; 0.0.0.0 ; 127.0.0.0/8 ; ::1

            # (debian) same_domain_copy_routing = yes
            # (debian) ignore private rfc1918 and APIPA addresses
            # (debian) ignore_target_hosts = 0.0.0.0 : 127.0.0.0/8 : 192.168.0.0/16 :\
            #                   172.16.0.0/12 : 10.0.0.0/8 : 169.254.0.0/16 :\
            #                   255.255.255.255

            # Fail and bounce if the router does not find the domain in the DNS.
            # I.e. no more routers are tried.
            # There are a few cases where a dnslookup router will decline to accept an
            # address; if such a router is expected to handle "all remaining non-local
            # domains", then it is important to set no_more.
            no_more

          # XXX this is only used because these "well known aliases" goto tv@cd.retiolum
          # TODO bounce everything, there is no @cd.retiolum
          system_aliases:
            debug_print = "R: system_aliases for $local_part@$domain"
            driver = redirect
            data = ''${lookup{$local_part}lsearch{${lsearch.system-aliases}}}

          # TODO this is only b/c mv here... send mv's mails somewhere else...
          local_user:
            debug_print = "R: local_user for $local_part@$domain"
            driver = accept
            check_local_user
          # local_part_suffix = +* : -*
          # local_part_suffix_optional
            transport = home_maildir
            cannot_route_message = Unknown user

          begin transports

          retiolum_smtp:
            driver = smtp
            retry_include_ip_address = false
            # serialize_hosts = TODO-all-slow-hosts

          remote_smtp:
            driver = smtp
            # debian has also stuff for tls, headers_rewrite and more here

            # feature HELO_REWRITE
            # XXX note that the public ip (162.219.5.183) resolves to viljetic.de
            helo_data = ''${if eq{$acl_m_special_dom}{}  \
                                 {$primary_hostname}   \
                                 {$acl_m_special_dom} }

          home_maildir:
            driver = appendfile
            maildir_format
            maildir_use_size_file
            directory = $home/Maildir
            directory_mode = 0700
            delivery_date_add
            envelope_to_add
            return_path_add

          begin retry
          *.retiolum             *           F,42d,1m
          *                      *           F,2h,15m; G,16h,1h,1.5; F,4d,6h

          begin rewrite
          begin authenticators
        '';


          # group = mail
          # mode = 0660


          #address_pipe:
          #  driver = pipe
          #  return_output
          #
          #address_file:
          #  driver = appendfile
          #  delivery_date_add
          #  envelope_to_add
          #  return_path_add
          #
          #address_reply:
          #  driver = autoreply


          #maildrop_pipe:
          #  debug_print = "T: maildrop_pipe for $local_part@$domain"
          #  driver = pipe
          #  path = "/bin:/usr/bin:/usr/local/bin"
          #  command = "/usr/bin/maildrop"
          #  return_path_add
          #  delivery_date_add
          #  envelope_to_add





          ##begin retry
          # Address or Domain    Error       Retries

          # Our host_redirect destinations might be offline a lot.
          # TODO define fallback destinations(?)
          #lsearch;${lsearch.internet-aliases} * F,42d,1m


          ## begin rewrite

          # just in case (shackspace.de should already do this)
          #tv@shackspace.de  tv@SHACK_REDIR_HOSTNAME  T


          ## begin authenticators
          #PLAIN:
          #  driver                  = plaintext
          #  server_set_id           = $auth2
          #  server_prompts          = :
          #  server_condition        = Authentication is not yet configured
          #  server_advertise_condition = ''${if def:tls_in_cipher }

          #LOGIN:
          #  driver                  = plaintext
          #  server_set_id           = $auth1
          #  server_prompts          = <| Username: | Password:
          #  server_condition        = Authentication is not yet configured
          #  server_advertise_condition = ''${if def:tls_in_cipher }



      };

}

#        config = ''
#          primary_hostname = ${retiolumHostname}
#          domainlist local_domains    = @ : localhost
#          domainlist relay_to_domains = *.retiolum
#          hostlist   relay_from_hosts = <; 127.0.0.1 ; ::1
#
#          acl_smtp_rcpt = acl_check_rcpt
#          acl_smtp_data = acl_check_data
#
#          host_lookup = *
#          rfc1413_hosts = *
#          rfc1413_query_timeout = 5s
#
#          log_file_path = syslog
#          syslog_timestamp = false
#          syslog_duplication = false
#
#          begin acl
#
#          acl_check_rcpt:
#            accept  hosts = :
#                    control = dkim_disable_verify
#
#            deny    message       = Restricted characters in address
#                    domains       = +local_domains
#                    local_parts   = ^[.] : ^.*[@%!/|]
#
#            deny    message       = Restricted characters in address
#                    domains       = !+local_domains
#                    local_parts   = ^[./|] : ^.*[@%!] : ^.*/\\.\\./
#
#            accept  local_parts   = postmaster
#                    domains       = +local_domains
#
#            #accept
#            #  hosts = *.retiolum
#            #  domains = *.retiolum
#            #  control = dkim_disable_verify
#
#            #require verify        = sender
#
#            accept  hosts         = +relay_from_hosts
#                    control       = submission
#                    control       = dkim_disable_verify
#
#            accept  authenticated = *
#                    control       = submission
#                    control       = dkim_disable_verify
#
#            require message = relay not permitted
#                    domains = +local_domains : +relay_to_domains
#
#            require verify = recipient
#
#            accept
#
#
#          acl_check_data:
#            accept
#
#
#          begin routers
#
#          retiolum:
#            driver = manualroute
#            domains = ! ${retiolumHostname} : *.retiolum
#            transport = remote_smtp
#            route_list = ^.* $0 byname
#            no_more
#
#          nonlocal:
#            debug_print = "R: nonlocal for $local_part@$domain"
#            driver = redirect
#            domains = ! +local_domains
#            allow_fail
#            data = :fail: Mailing to remote domains not supported
#            no_more
#
#          local_user:
#            # debug_print = "R: local_user for $local_part@$domain"
#            driver = accept
#            check_local_user
#          # local_part_suffix = +* : -*
#          # local_part_suffix_optional
#            transport = home_maildir
#            cannot_route_message = Unknown user
#
#
#          begin transports
#
#          remote_smtp:
#            driver = smtp
#
#          home_maildir:
#            driver = appendfile
#            maildir_format
#            directory = $home/Maildir
#            directory_mode = 0700
#            delivery_date_add
#            envelope_to_add
#            return_path_add
#          # group = mail
#          # mode = 0660
#
#          begin retry
#          *.retiolum             *           F,42d,1m
#          *                      *           F,2h,15m; G,16h,1h,1.5; F,4d,6h
#
#          begin rewrite
#
#          begin authenticators
#        '';
#      };
#}
