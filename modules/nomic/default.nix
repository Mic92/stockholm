{ config, pkgs, ... }:

let
  location = pkgs.lib.nameValuePair; # TODO this is also in modules/tv/git/cgit.nix
in

{
  imports = [
    ./hardware-configuration.nix
    ./users.nix
    ../tv/base.nix
    ../tv/exim-retiolum.nix
    ../tv/git/public.nix
    ../tv/sanitize.nix
    ../tv/smartd.nix
    {
      imports = [ ../tv/iptables ];
      tv.iptables = {
        enable = true;
        input-internet-accept-new-tcp = [
          "ssh"
          "http"
          "tinc"
          "smtp"
        ];
      };
    }
    {
      imports = [ ../tv/nginx ];
      tv.nginx = {
        enable = true;
        retiolum-locations = [
          (location "~ ^/~(.+?)(/.*)?\$" ''
            alias /home/$1/public_html$2;
          '')
        ];
      };
    }
    {
      imports = [ ../tv/retiolum ];
      tv.retiolum = {
        enable = true;
        hosts = <retiolum-hosts>;
        connectTo = [
          "gum"
          "pigstarter"
        ];
      };
    }
  ];

  boot.kernel.sysctl = {
    # Enable IPv6 Privacy Extensions
    "net.ipv6.conf.all.use_tempaddr" = 2;
    "net.ipv6.conf.default.use_tempaddr" = 2;
  };

  networking = {
    hostName = "nomic";
    wireless.enable = true;
  };

  services.openssh = {
    enable = true;
    hostKeys = [
      { type = "ed25519"; path = "/etc/ssh/ssh_host_ed25519_key"; }
    ];
  };
}
