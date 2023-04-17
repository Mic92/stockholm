{ config, pkgs, ... }:

{
  networking.firewall.allowedTCPPorts = [
    6667
  ];

  services.ergochat = {
    enable = true;
    settings = {
      server.name = "irc.r";
      server.secure-nets = [
        "42::0/16"
        "10.240.0.0/12"
      ];
      oper-classes.server-admin = {
        title = "admin";
        capabilities = [
          "kill"         # disconnect user sessions
          "ban"          # ban IPs, CIDRs, and NUH masks ("d-line" and "k-line")
          "nofakelag"    # remove "fakelag" restrictions on rate of message sending
          "relaymsg"     # use RELAYMSG in any channel (see the 'relaymsg' config block)
          "vhosts"       # add and remove vhosts from users
          "sajoin"       # join arbitrary channels, including private channels
          "samode"       # modify arbitrary channel and user modes
          "snomasks"     # subscribe to arbitrary server notice masks
          "roleplay"     # use the (deprecated) roleplay commands in any channel
          "rehash"       # rehash the server, i.e. reload the config at runtime
          "accreg"       # modify arbitrary account registrations
          "chanreg"      # modify arbitrary channel registrations
          "history"      # modify or delete history messages
          "defcon"       # use the DEFCON command (restrict server capabilities)
          "massmessage"  # message all users on the server
        ];
      };
      opers.aids = {
        class = "server-admin";
        hidden = false;
        password = "$2a$04$0AtVycWQJ07ymrDdKyAm2un3UVSVIzpzL3wsWbWb3PF95d1CZMcMO";
      };
      server.max-line-length = 1024;
      server.lookup-hostnames = true;
    };
  };
}


