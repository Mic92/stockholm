with import <stockholm/lib>;
{ config, ... }: let

  format = from: to: {
    inherit from;
    # TODO assert is-retiolum-mail-address to;
    to = concatMapStringsSep "," (getAttr "mail") (toList to);
  };

in {
  krebs.exim-smarthost.internet-aliases =
    mapAttrsToList format (with config.krebs.users; let
      brain-ml = [
        lass
        makefu
        tv
      ];
      eloop-ml = spam-ml;
      spam-ml = [
        lass
        makefu
        tv
      ];
    in {
      "anmeldung@eloop.org" = eloop-ml;
      "brain@krebsco.de" = brain-ml;
      "cfp2019@eloop.org" = eloop-ml;
      "eloop2022@krebsco.de" = eloop-ml;
      "kontakt@eloop.org" = eloop-ml;
      "root@eloop.org" = eloop-ml;
      "youtube@eloop.org" = eloop-ml;
      "postmaster@krebsco.de" = spam-ml; # RFC 822
      "lass@krebsco.de" = lass;
      "makefu@krebsco.de" = makefu;
      "spam@krebsco.de" = spam-ml;
      "tv@krebsco.de" = tv;
      # XXX These are no internet aliases
      # XXX exim-retiolum hosts should be able to relay to retiolum addresses
      "lass@retiolum" = lass;
      "makefu@retiolum" = makefu;
      "spam@retiolum" = spam-ml;
      "tv@retiolum" = tv;
      "lass@r" = lass;
      "makefu@r" = makefu;
      "spam@r" = spam-ml;
      "tv@r" = tv;
    });
}
