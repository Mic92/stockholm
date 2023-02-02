with import ./lib;
{ config, pkgs, ... }: let
  cfg = {
    enable = cfg.net != null;
    net = config.krebs.build.host.nets.wiregrill or null;
  };
  toCidrNotation = ip: "${ip.addr}/${toString ip.prefixLength}";
in
  mkIf cfg.enable {
    networking.wireguard.interfaces.wiregrill = {
      ips =
        optional (cfg.net.ip4 != null) cfg.net.ip4.addr ++
        optional (cfg.net.ip6 != null) cfg.net.ip6.addr;
      listenPort = 51820;
      privateKeyFile = (toString <secrets>) + "/wiregrill.key";
      allowedIPsAsRoutes = true;
      peers = mapAttrsToList
        (_: host: {
          allowedIPs = host.nets.wiregrill.wireguard.subnets;
          endpoint =
            mkIf (host.nets.wiregrill.via != null) (host.nets.wiregrill.via.ip4.addr + ":${toString host.nets.wiregrill.wireguard.port}");
          persistentKeepalive = mkIf (host.nets.wiregrill.via != null) 61;
          publicKey =
            replaceStrings ["\n"] [""] host.nets.wiregrill.wireguard.pubkey;
        })
        (filterAttrs (_: h: hasAttr "wiregrill" h.nets) config.krebs.hosts);
    };
    systemd.network.networks.wiregrill = {
      matchConfig.Name = "wiregrill";
      address =
        optional (!isNull cfg.net.ip4) (toCidrNotation cfg.net.ip4) ++
        optional (!isNull cfg.net.ip6) (toCidrNotation cfg.net.ip6);
    };
    tv.iptables.extra.filter.INPUT = [
      "-p udp --dport ${toString cfg.net.wireguard.port} -j ACCEPT"
    ];
  }
