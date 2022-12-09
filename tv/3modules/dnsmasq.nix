with import ./lib;
{ config, ... }: let
  cfg = config.tv.dnsmasq;
in {

  options.tv.dnsmasq = {
    enable = mkEnableOption "tv.dnsmasq";
    dhcp-range = mkOption {
      type = types.str;
    };
    interface = mkOption {
      type = types.str;
    };
    address = mkOption {
      type = types.str;
    };
    prefixLength = mkOption {
      type = types.addCheck types.int (x: x >= 0 && x <= 32);
    };
  };

  config = mkIf cfg.enable (mkMerge [
    {
      networking.dhcpcd.denyInterfaces = [ cfg.interface ];
      services.dnsmasq.resolveLocalQueries = false;
      networking.interfaces.${cfg.interface} = {
        ipv4.addresses = singleton {
          address = cfg.address;
          prefixLength = cfg.prefixLength;
        };
      };
      services.dnsmasq.enable = true;
      services.dnsmasq.extraConfig = ''
        bind-interfaces
        dhcp-range=${cfg.dhcp-range}
        listen-address=${cfg.address}
      '';
      tv.iptables.extra.filter.INPUT = [
        "-i ${cfg.interface} -p tcp -m tcp --dport bootps -j ACCEPT"
        "-i ${cfg.interface} -p udp -m udp --dport bootps -j ACCEPT"
        "-i ${cfg.interface} -p tcp -m tcp --dport domain -j ACCEPT"
        "-i ${cfg.interface} -p udp -m udp --dport domain -j ACCEPT"
      ];
    }
    {
      # enable forwarding
      boot.kernel.sysctl."net.ipv4.ip_forward" = true;
      tv.iptables.extra.filter.FORWARD = [
        "-m state --state RELATED,ESTABLISHED -j ACCEPT"
        "-i ${cfg.interface} -j ACCEPT"
      ];
      tv.iptables.extra.nat.POSTROUTING = [
        "-j MASQUERADE"
      ];
    }
  ]);

}
