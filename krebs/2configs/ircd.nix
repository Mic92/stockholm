{ config, pkgs, lib, ... }:

{
  networking.firewall.allowedTCPPorts = [
    6667 6669
  ];

  systemd.services.solanum.serviceConfig.LimitNOFILE = lib.mkForce 16384;

  services.solanum = {
    enable = true;
    motd = ''
      hello
    '';
    config = ''
      loadmodule "extensions/m_omode";
      serverinfo {
        name = "${config.krebs.build.host.name}.irc.r";
        sid = "1as";
        description = "irc!";
        network_name = "irc.r";

        vhost = "0.0.0.0";
        vhost6 = "::";

        #ssl_private_key = "etc/ssl.key";
        #ssl_cert = "etc/ssl.cert";
        #ssl_dh_params = "etc/dh.pem";
        #ssld_count = 1;

        default_max_clients = 2048;
        #nicklen = 30;
      };

      listen {
        defer_accept = yes;

        /* If you want to listen on a specific IP only, specify host.
         * host definitions apply only to the following port line.
         */
        host = "0.0.0.0";
        port = 6667;
        #sslport = 6697;

        /* Listen on IPv6 (if you used host= above). */
        host = "::";
        port = 6667;
        #sslport = 6697;
      };

      class "users" {
        ping_time = 2 minutes;
        number_per_ident = 10;
        number_per_ip = 4096;
        number_per_ip_global = 4096;
        cidr_ipv4_bitlen = 24;
        cidr_ipv6_bitlen = 64;
        number_per_cidr = 65535;
        max_number = 65535;
        sendq = 1000 megabyte;
      };

      privset "op" {
        privs = oper:admin, oper:general;
      };

      operator "aids" {
        user = "*@*";
        password = "balls";
        flags = ~encrypted;
        snomask = "+s";
        privset = "op";
      };

      exempt {
        ip = "127.0.0.1";
      };

      exempt {
        ip = "10.243.0.0/16";
      };

      auth {
        user = "*@*";
        class = "users";
        flags = kline_exempt, exceed_limit, flood_exempt;
      };

      channel {
        autochanmodes = "+t";
        use_invex = yes;
        use_except = yes;
        use_forward = yes;
        use_knock = yes;
        knock_delay = 5 minutes;
        knock_delay_channel = 1 minute;
        max_chans_per_user = 150;
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
        default_floodcount = 10000;
        disable_auth = yes;
        throttle_duration = 1;
        throttle_count = 10000;
      };
    '';
  };
}
