with import <stockholm/lib>;
{ config, pkgs, ... }: let

  self = config.krebs.build.host.nets.wirelum;
  isRouter = !isNull self.via;

in mkIf (hasAttr "wirelum" config.krebs.build.host.nets) {
  #hack for modprobe inside containers
  systemd.services."wireguard-wirelum".path = mkIf config.boot.isContainer (mkBefore [
    (pkgs.writeDashBin "modprobe" ":")
  ]);

  boot.kernel.sysctl = mkIf isRouter {
    "net.ipv6.conf.all.forwarding" = 1;
  };
  krebs.iptables.tables.filter.INPUT.rules = [
     { predicate = "-p udp --dport ${toString self.wireguard.port}"; target = "ACCEPT"; }
  ];
  krebs.iptables.tables.filter.FORWARD.rules = mkIf isRouter [
    { precedence = 1000; predicate = "-i wirelum -o wirelum"; target = "ACCEPT"; }
  ];

  networking.wireguard.interfaces.wirelum = {
    ips =
      (optional (!isNull self.ip4) self.ip4.addr) ++
      (optional (!isNull self.ip6) self.ip6.addr);
    listenPort = 51820;
    privateKeyFile = (toString <secrets>) + "/wirelum.key";
    allowedIPsAsRoutes = true;
    peers = mapAttrsToList
      (_: host: {
        allowedIPs = if isRouter then
          (optional (!isNull host.nets.wirelum.ip4) host.nets.wirelum.ip4.addr) ++
          (optional (!isNull host.nets.wirelum.ip6) host.nets.wirelum.ip6.addr)
        else
          host.nets.wirelum.wireguard.subnets
        ;
        endpoint = mkIf (!isNull host.nets.wirelum.via) (host.nets.wirelum.via.ip4.addr + ":${toString host.nets.wirelum.wireguard.port}");
        persistentKeepalive = mkIf (!isNull host.nets.wirelum.via) 61;
        publicKey = host.nets.wirelum.wireguard.pubkey;
      })
      (filterAttrs (_: h: hasAttr "wirelum" h.nets) config.krebs.hosts);
  };
}
