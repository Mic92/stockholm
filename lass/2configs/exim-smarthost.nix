{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

{
  krebs.exim-smarthost = {
    enable = true;
    dkim = [
      { domain = "lassul.us"; }
    ];
    primary_hostname = "lassul.us";
    sender_domains = [
      "lassul.us"
      "aidsballs.de"
    ];
    relay_from_hosts = map (host: host.nets.retiolum.ip6.addr) [
      config.krebs.hosts.mors
      config.krebs.hosts.uriel
    ];
    internet-aliases = with config.krebs.users; [
      { from = "postmaster@lassul.us"; to = lass.mail; } # RFC 822
      { from = "lass@lassul.us"; to = lass.mail; }
      { from = "lassulus@lassul.us"; to = lass.mail; }
      { from = "test@lassul.us"; to = lass.mail; }
      { from = "outlook@lassul.us"; to = lass.mail; }
      { from = "steuer@aidsballs.de"; to = lass.mail; }
      { from = "lass@aidsballs.de"; to = lass.mail; }
      { from = "wordpress@ubikmedia.de"; to = lass.mail; }
      { from = "finanzamt@lassul.us"; to = lass.mail; }
      { from = "netzclub@lassul.us"; to = lass.mail; }
      { from = "nebenan@lassul.us"; to = lass.mail; }
      { from = "feed@lassul.us"; to = lass.mail; }
      { from = "art@lassul.us"; to = lass.mail; }
      { from = "irgendwas@lassul.us"; to = lass.mail; }
      { from = "polo@lassul.us"; to = lass.mail; }
      { from = "shack@lassul.us"; to = lass.mail; }
      { from = "nix@lassul.us"; to = lass.mail; }
      { from = "c-base@lassul.us"; to = lass.mail; }
      { from = "paypal@lassul.us"; to = lass.mail; }
      { from = "patreon@lassul.us"; to = lass.mail; }
      { from = "steam@lassul.us"; to = lass.mail; }
      { from = "securityfocus@lassul.us"; to = lass.mail; }
      { from = "radio@lassul.us"; to = lass.mail; }
    ];
    system-aliases = [
      { from = "mailer-daemon"; to = "postmaster"; }
      { from = "postmaster"; to = "root"; }
      { from = "nobody"; to = "root"; }
      { from = "hostmaster"; to = "root"; }
      { from = "usenet"; to = "root"; }
      { from = "news"; to = "root"; }
      { from = "webmaster"; to = "root"; }
      { from = "www"; to = "root"; }
      { from = "ftp"; to = "root"; }
      { from = "abuse"; to = "root"; }
      { from = "noc"; to = "root"; }
      { from = "security"; to = "root"; }
      { from = "root"; to = "lass"; }
    ];
  };
  krebs.iptables.tables.filter.INPUT.rules = [
    { predicate = "-p tcp --dport smtp"; target = "ACCEPT"; }
  ];
}
