{ config, lib, pkgs, ... }:
{
  services.postgresql = {
    enable = true;
    dataDir = "/var/state/postgresql/${config.services.postgresql.package.psqlSchema}";
    package = pkgs.postgresql_11;
  };
  systemd.tmpfiles.rules = [
    "d /var/state/postgresql 0700 postgres postgres -"
  ];

  services.mastodon = {
    enable = true;
    localDomain = "social.krebsco.de";
    configureNginx = true;
    trustedProxy = config.krebs.hosts.prism.nets.retiolum.ip6.addr;
    smtp.createLocally = false;
    smtp.fromAddress = "derp";
  };

  services.nginx.virtualHosts.${config.services.mastodon.localDomain} = {
    forceSSL = lib.mkForce false;
    enableACME = lib.mkForce false;
    locations."@proxy".extraConfig = ''
      proxy_redirect off;
      proxy_pass_header Server;
      proxy_set_header X-Forwarded-Proto $http_x_forwarded_proto;
    '';
  };

  networking.firewall.allowedTCPPorts = [
    80
  ];

  environment.systemPackages = [
    (pkgs.writers.writeDashBin "tootctl" ''
      sudo -u mastodon /etc/profiles/per-user/mastodon/bin/mastodon-env /etc/profiles/per-user/mastodon/bin/tootctl "$@"
    '')
    (pkgs.writers.writeDashBin "create-mastodon-user" ''
      set -efu
      nick=$1
      /run/current-system/sw/bin/tootctl accounts create "$nick" --email "$nick"@krebsco.de --confirmed
      /run/current-system/sw/bin/tootctl accounts approve "$nick"
    '')
  ];
}
