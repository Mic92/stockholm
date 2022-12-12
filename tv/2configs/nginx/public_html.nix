with import ./lib;
{ config, ... }: {
  services.nginx = {
    enable = true;
    virtualHosts.default = {
      serverAliases = [
        "localhost"
        "${config.krebs.build.host.name}"
        "${config.krebs.build.host.name}.hkw"
        "${config.krebs.build.host.name}.r"
      ];
      locations."~ ^/~([a-z]+)(?:/(.*))?\$" = {
        alias = "/srv/$1/public_html/$2";
      };
    };
  };
  tv.iptables.input-internet-accept-tcp = singleton "http";
}
