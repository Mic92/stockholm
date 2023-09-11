{ pkgs, lib, ... }:
with lib;
let
  domain = "cache.nsupdate.info";
in {
  # This only works for a single domain for nsupdate.info as multiple usernames
  # and passwords are required for multiple domains
  services.ddclient = {
    enable = true;
    server = "ipv4.nsupdate.info";
    username = domain;
    password = import "${config.krebs.secret.directory}/nsupdate-cache.nix";
    domains = [ domain ];
    use= "if, if=et0";
    # use = "web, web=http://ipv4.nsupdate.info/myip";

  };
  krebs.cachecache = {
    enable = true;
    enableSSL = true; # disable letsencrypt for testing
    cacheDir = "/var/cache/nix-cache-cache";
    maxSize = "10g";
    indexFile = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/krebs/35c3-nixos-cache/master/index.html";
      sha256 = "1vlngzbn0jipigspccgikd7xgixksimdl4wf8ix7d30ljx47p9n0";
    };

    # assumes that the domain is reachable from the internet
    virtualHost = domain;
  };

  boot.kernelModules = [ "tcp_bbr" ];

  boot.kernel.sysctl."net.ipv4.tcp_congestion_control" = "bbr";
  boot.kernel.sysctl."net.core.default_qdisc" = "fq";
  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
