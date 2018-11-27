{lib, ... }:
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
    password = import ((toString <secrets>) + "/nsupdate-cache.nix");
    domains = [ domain ];
    use= "if, if=et0";
    # use = "web, web=http://ipv4.nsupdate.info/myip";

  };
  krebs.cachecache = {
    enable = true;
    enableSSL = false; # disable letsencrypt for testing
    cacheDir = "/var/cache/nix-cache-cache";
    maxSize = "10g";

    # assumes that the domain is reachable from the internet
    virtualHost = domain;
  };

  boot.kernelModules = [ "tcp_bbr" ];

  boot.kernel.sysctl."net.ipv4.tcp_congestion_control" = "bbr";
  boot.kernel.sysctl."net.core.default_qdisc" = "fq";
  networking.firewall.allowedTCPPorts = [ 80 443 ];
}
