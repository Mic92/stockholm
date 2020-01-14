{ config, lib, pkgs, ... }: with import <stockholm/lib>; let

  to = concatStringsSep "," [
    "lass@blue.r"
    "lass@xerxes.r"
    "lass@mors.r"
  ];

  mails = [
    "postmaster@lassul.us"
    "lass@lassul.us"
    "lassulus@lassul.us"
    "test@lassul.us"
    "outlook@lassul.us"
    "steuer@aidsballs.de"
    "lass@aidsballs.de"
    "wordpress@ubikmedia.de"
    "finanzamt@lassul.us"
    "netzclub@lassul.us"
    "nebenan@lassul.us"
    "feed@lassul.us"
    "art@lassul.us"
    "irgendwas@lassul.us"
    "polo@lassul.us"
    "shack@lassul.us"
    "nix@lassul.us"
    "c-base@lassul.us"
    "paypal@lassul.us"
    "patreon@lassul.us"
    "steam@lassul.us"
    "securityfocus@lassul.us"
    "radio@lassul.us"
    "btce@lassul.us"
    "raf@lassul.us"
    "apple@lassul.us"
    "coinbase@lassul.us"
    "tomtop@lassul.us"
    "aliexpress@lassul.us"
    "business@lassul.us"
    "payeer@lassul.us"
    "github@lassul.us"
    "bitwala@lassul.us"
    "bitstamp@lassul.us"
    "bitcoin.de@lassul.us"
    "ableton@lassul.us"
    "dhl@lassul.us"
    "sipgate@lassul.us"
    "coinexchange@lassul.us"
    "verwaltung@lassul.us"
    "gearbest@lassul.us"
    "binance@lassul.us"
    "bitfinex@lassul.us"
    "alternate@lassul.us"
    "redacted@lassul.us"
    "mytaxi@lassul.us"
    "pizza@lassul.us"
    "robinhood@lassul.us"
    "drivenow@lassul.us"
    "aws@lassul.us"
    "reddit@lassul.us"
    "banggood@lassul.us"
    "immoscout@lassul.us"
    "gmail@lassul.us"
    "amazon@lassul.us"
    "humblebundle@lassul.us"
    "meetup@lassul.us"
    "gebfrei@lassul.us"
    "github@lassul.us"
    "ovh@lassul.us"
    "hetzner@lassul.us"
    "allygator@lassul.us"
    "immoscout@lassul.us"
    "elitedangerous@lassul.us"
    "boardgamegeek@lassul.us"
    "qwertee@lassul.us"
    "zazzle@lassul.us"
    "hackbeach@lassul.us"
    "transferwise@lassul.us"
    "cis@lassul.us"
    "afra@lassul.us"
    "ksp@lassul.us"
    "ccc@lassul.us"
    "neocron@lassul.us"
    "osmocom@lassul.us"
    "lesswrong@lassul.us"
    "nordvpn@lassul.us"
    "csv-direct@lassul.us"
    "nintendo@lassul.us"
    "overleaf@lassul.us"
    "box@lassul.us"
    "paloalto@lassul.us"
    "subtitles@lassul.us"
    "lobsters@lassul.us"
    "fysitech@lassul.us"
    "threema@lassul.us"
    "ubisoft@lassul.us"
    "kottezeller@lassul.us"
    "pie@lassul.us"
    "vebit@lassul.us"
    "vcvrack@lassul.us"
    "epic@lassul.us"
    "microsoft@lassul.us"
    "stickers@lassul.us"
    "nextbike@lassul.us"
  ];

in {
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
      config.krebs.hosts.xerxes
    ];
    internet-aliases = map (from: { inherit from to; }) mails;
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
