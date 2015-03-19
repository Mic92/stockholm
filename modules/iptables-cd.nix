{ config, pkgs, ... }:

{
  #
  # iptables
  #
  networking.firewall.enable = false;
  system.activationScripts.iptables =
    let
      log = false;
      when = c: f: if c then f else "";
    in
      ''
        ip4tables() { ${pkgs.iptables}/sbin/iptables "$@"; }
        ip6tables() { ${pkgs.iptables}/sbin/ip6tables "$@"; }
        ipXtables() { ip4tables "$@" && ip6tables "$@"; }

        # XXX This fails with the original CAC CentOS 7 kernel.
        if ipXtables -vL >/dev/null; then

          #
          # nat
          #

          # reset tables
          ipXtables -t nat -F
          ipXtables -t nat -X

          #
          ipXtables -t nat -A PREROUTING -j REDIRECT ! -i retiolum -p tcp --dport ssh --to-ports 0
          ipXtables -t nat -A PREROUTING -j REDIRECT -p tcp --dport 11423 --to-ports ssh

          #
          # filter
          #

          # reset tables
          ipXtables -P INPUT DROP
          ipXtables -P FORWARD DROP
          ipXtables -F
          ipXtables -X

          # create custom chains
          ipXtables -N Retiolum

          # INPUT
          ipXtables -A INPUT -j ACCEPT -m conntrack --ctstate RELATED,ESTABLISHED
          ipXtables -A INPUT -j ACCEPT -i lo
          ipXtables -A INPUT -j ACCEPT -p tcp --dport ssh -m conntrack --ctstate NEW
          #ipXtables -A INPUT -j ACCEPT -p tcp --dport http -m conntrack --ctstate NEW
          ipXtables -A INPUT -j ACCEPT -p tcp --dport tinc -m conntrack --ctstate NEW
          ipXtables -A INPUT -j ACCEPT -p tcp --dport smtp -m conntrack --ctstate NEW
          #ipXtables -A INPUT -j ACCEPT -p tcp --dport xmpp-client -m conntrack --ctstate NEW
          #ipXtables -A INPUT -j ACCEPT -p tcp --dport xmpp-server -m conntrack --ctstate NEW

          ipXtables -A INPUT -j Retiolum -i retiolum
          ${when log "ipXtables -A INPUT -j LOG --log-level info --log-prefix 'INPUT DROP '"}

          # FORWARD
          ${when log "ipXtables -A FORWARD -j LOG --log-level info --log-prefix 'FORWARD DROP '"}

          # Retiolum
          ip4tables -A Retiolum -j ACCEPT -p icmp --icmp-type echo-request
          ip6tables -A Retiolum -j ACCEPT -p ipv6-icmp -m icmp6 --icmpv6-type echo-request


          ${when log "ipXtables -A Retiolum -j LOG --log-level info --log-prefix 'REJECT '"}
          ipXtables -A Retiolum -j REJECT -p tcp --reject-with tcp-reset
          ip4tables -A Retiolum -j REJECT -p udp --reject-with icmp-port-unreachable
          ip4tables -A Retiolum -j REJECT        --reject-with icmp-proto-unreachable
          ip6tables -A Retiolum -j REJECT -p udp --reject-with icmp6-port-unreachable
          ip6tables -A Retiolum -j REJECT
        fi
      '';
}
