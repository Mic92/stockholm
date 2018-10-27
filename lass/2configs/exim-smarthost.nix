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
    ];
    relay_from_hosts = map (host: host.nets.retiolum.ip6.addr) [
      config.krebs.hosts.mors
      config.krebs.hosts.blue
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
      { from = "btce@lassul.us"; to = lass.mail; }
      { from = "raf@lassul.us"; to = lass.mail; }
      { from = "apple@lassul.us"; to = lass.mail; }
      { from = "coinbase@lassul.us"; to = lass.mail; }
      { from = "tomtop@lassul.us"; to = lass.mail; }
      { from = "aliexpress@lassul.us"; to = lass.mail; }
      { from = "business@lassul.us"; to = lass.mail; }
      { from = "payeer@lassul.us"; to = lass.mail; }
      { from = "github@lassul.us"; to = lass.mail; }
      { from = "bitwala@lassul.us"; to = lass.mail; }
      { from = "bitstamp@lassul.us"; to = lass.mail; }
      { from = "bitcoin.de@lassul.us"; to = lass.mail; }
      { from = "ableton@lassul.us"; to = lass.mail; }
      { from = "dhl@lassul.us"; to = lass.mail; }
      { from = "sipgate@lassul.us"; to = lass.mail; }
      { from = "coinexchange@lassul.us"; to = lass.mail; }
      { from = "verwaltung@lassul.us"; to = lass.mail; }
      { from = "gearbest@lassul.us"; to = lass.mail; }
      { from = "binance@lassul.us"; to = lass.mail; }
      { from = "bitfinex@lassul.us"; to = lass.mail; }
      { from = "alternate@lassul.us"; to = lass.mail; }
      { from = "redacted@lassul.us"; to = lass.mail; }
      { from = "mytaxi@lassul.us"; to = lass.mail; }
      { from = "pizza@lassul.us"; to = lass.mail; }
      { from = "robinhood@lassul.us"; to = lass.mail; }
      { from = "drivenow@lassul.us"; to = lass.mail; }
      { from = "aws@lassul.us"; to = lass.mail; }
      { from = "reddit@lassul.us"; to = lass.mail; }
      { from = "banggood@lassul.us"; to = lass.mail; }
      { from = "immoscout@lassul.us"; to = lass.mail; }
      { from = "gmail@lassul.us"; to = lass.mail; }
      { from = "amazon@lassul.us"; to = lass.mail; }
      { from = "humblebundle@lassul.us"; to = lass.mail; }
      { from = "meetup@lassul.us"; to = lass.mail; }
      { from = "gebfrei@lassul.us"; to = lass.mail; }
      { from = "github@lassul.us"; to = lass.mail; }
      { from = "ovh@lassul.us"; to = lass.mail; }
      { from = "hetzner@lassul.us"; to = lass.mail; }
      { from = "allygator@lassul.us"; to = lass.mail; }
      { from = "immoscout@lassul.us"; to = lass.mail; }
      { from = "elitedangerous@lassul.us"; to = lass.mail; }
      { from = "boardgamegeek@lassul.us"; to = lass.mail; }
      { from = "qwertee@lassul.us"; to = lass.mail; }
      { from = "zazzle@lassul.us"; to = lass.mail; }
      { from = "hackbeach@lassul.us"; to = lass.mail; }
      { from = "transferwise@lassul.us"; to = lass.mail; }
      { from = "cis@lassul.us"; to = lass.mail; }
      { from = "afra@lassul.us"; to = lass.mail; }
      { from = "ksp@lassul.us"; to = lass.mail; }
      { from = "ccc@lassul.us"; to = lass.mail; }
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
