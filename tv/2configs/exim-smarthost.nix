with import ./lib;
{ config, pkgs, ... }: {
  environment.systemPackages = [
    pkgs.eximlog
  ];
  krebs.exim-smarthost = {
    enable = true;
    dkim = [
      { domain = "viljetic.de"; }
    ];
    sender_domains = [
      "krebsco.de"
      "shackspace.de"
      "viljetic.de"
    ];
    relay_from_hosts = concatMap (host: host.nets.retiolum.addrs) [
      config.krebs.hosts.nomic
      config.krebs.hosts.wu
      config.krebs.hosts.xu
    ];
    internet-aliases = with config.krebs.users; [
      { from = "bku-eppler@viljetic.de"; to = tv.mail; }
      { from = "postmaster@viljetic.de"; to = tv.mail; } # RFC 822
      { from = "mirko@viljetic.de"; to = mv-ni.mail; }
      { from = "tomislav@viljetic.de"; to = tv.mail; }
      { from = "tv@viljetic.de"; to = tv.mail; }
      { from = "tv@shackspace.de"; to = tv.mail; }
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
      { from = "root"; to = "tv"; }
      { from = "mirko"; to = "mv"; }
    ];
  };
  tv.iptables.input-internet-accept-tcp = singleton "smtp";
}
