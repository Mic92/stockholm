{ config, lib, pkgs, ... }:

with config.krebs.lib;

{
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
    relay_from_hosts = concatMap (host: host.nets.retiolum.ip4.addr) [
      config.krebs.hosts.nomic
      config.krebs.hosts.wu
      config.krebs.hosts.xu
    ];
    internet-aliases = with config.krebs.users; [
      { from = "postmaster@viljetic.de"; to = tv.mail; } # RFC 822
      { from = "mirko@viljetic.de"; to = mv.mail; }
      { from = "tomislav@viljetic.de"; to = tv.mail; }
      { from = "tv@destroy.dyn.shackspace.de"; to = tv.mail; }
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
  krebs.setuid.sendmail = {
    filename = "${pkgs.exim}/bin/exim";
    mode = "4111";
  };
  tv.iptables.input-internet-accept-new-tcp = singleton "smtp";
}
