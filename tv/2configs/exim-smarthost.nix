{ config, pkgs, ... }:

{
  krebs.exim-smarthost = {
    enable = true;
    primary_hostname = "${config.networking.hostName}.retiolum";
    sender_domains = [
      "shackspace.de"
      "viljetic.de"
    ];
    relay_from_hosts = [
      "10.243.13.37"
    ];
    internet-aliases = with config.krebs.users; [
      { from = "tomislav@viljetic.de"; to = tv.mail; }

      # (mindestens) lisp-stammtisch und elli haben die:
      { from = "tv@viljetic.de"; to = tv.mail; }

      { from = "tv@destroy.dyn.shackspace.de"; to = tv.mail; }

      { from = "mirko@viljetic.de"; to = mv.mail; }

      # TODO killme (wo wird die benutzt?)
      { from = "tv@cd.retiolum"; to = tv.mail; }

      # TODO lists@smtp.retiolum [consul]
      { from = "postmaster@krebsco.de"; to = tv.mail; }
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
}
