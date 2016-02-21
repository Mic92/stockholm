{ config, lib, pkgs, ... }:

with config.krebs.lib;

{
  krebs.exim-smarthost = {
    enable = true;
    sender_domains = [
      "shackspace.de"
      "viljetic.de"
    ];
    relay_from_hosts = [
      "10.243.13.37"
    ];
    internet-aliases = with config.krebs.users; [
      { from = "mirko@viljetic.de"; to = mv.mail; }
      { from = "tomislav@viljetic.de"; to = tv.mail; }
      { from = "tv@destroy.dyn.shackspace.de"; to = tv.mail; }
      { from = "tv@viljetic.de"; to = tv.mail; }
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
  tv.iptables.input-internet-accept-new-tcp = singleton "smtp";
}
