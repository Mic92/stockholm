{ config, pkgs, ... }:

{
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-i retiolum -p tcp --dport 6667"; target = "ACCEPT"; }
  ];
  config.services.charybdis = {
    enable = true;
    config = ''
      serverinfo {
        name = "${config.krebs.build.host.name}.irc.retiolum";
        sid = "1as";
        description = "miep!";
        network_name = "irc.retiolum";
        network_desc = "Retiolum IRC Network";
        hub = yes;

        vhost = "0.0.0.0";
        vhost6 = "::";

        #ssl_private_key = "etc/ssl.key";
        #ssl_cert = "etc/ssl.cert";
        #ssl_dh_params = "etc/dh.pem";
        #ssld_count = 1;

        default_max_clients = 10000;
        #nicklen = 30;
      };

      listen {
        defer_accept = yes;

        /* If you want to listen on a specific IP only, specify host.
         * host definitions apply only to the following port line.
         */
        host = "0.0.0.0";
        port = 6667;
        sslport = 6697;

        /* Listen on IPv6 (if you used host= above). */
        host = "::";
        port = 6667;
        sslport = 9999;
      };

      class "users" {
        ping_time = 2 minutes;
        number_per_ident = 200;
        number_per_ip = 200;
        number_per_ip_global = 500;
        cidr_ipv4_bitlen = 24;
        cidr_ipv6_bitlen = 64;
        number_per_cidr = 9000;
        max_number = 10000;
        sendq = 400 kbytes;
      };

      exempt {
        ip = "127.0.0.1";
      };

      auth {
        user = "*@*";
        class = "users";
        flags = exceed_limit;
      };

      channel {
        use_invex = yes;
        use_except = yes;
        use_forward = yes;
        use_knock = yes;
        knock_delay = 5 minutes;
        knock_delay_channel = 1 minute;
        max_chans_per_user = 15;
        max_bans = 100;
        max_bans_large = 500;
        default_split_user_count = 0;
        default_split_server_count = 0;
        no_create_on_split = no;
        no_join_on_split = no;
        burst_topicwho = yes;
        kick_on_split_riding = no;
        only_ascii_channels = no;
        resv_forcepart = yes;
        channel_target_change = yes;
        disable_local_channels = no;
      };
      general {
        #maybe we want ident someday?
        disable_auth = yes;
      };
    '';
  };
}
