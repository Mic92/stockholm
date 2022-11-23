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
      krebstel-ml = [
        config.krebs.users."0x4A6F"
        { mail = "krebstel-1rxz0mqa95nkmk298s1731ly0ii7vc36kkm36pnjj89hrq52pgn1@ni.r"; }
        { mail = "krebstel-1difh7483axpiaq92ghi14r5cql822wbhixqb0nn3y3jkcj0b785@ni.r"; }
        { mail = "lass@green.r"; }
        tv
        xkey
      ];
      spam-ml = [
        lass
        makefu
        tv
      ];
    in {
      "brain@krebsco.de" = brain-ml;
      "eloop2022@krebsco.de" = eloop-ml;
      "root@eloop.org" = eloop-ml; # obsolete, use spam@eloop.org instead
      "spam@eloop.org" = eloop-ml;
      "youtube@eloop.org" = eloop-ml; # obsolete, use spam@eloop.org instead
      "postmaster@krebsco.de" = spam-ml; # RFC 822
      "krebstel@krebsco.de" = krebstel-ml;
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
