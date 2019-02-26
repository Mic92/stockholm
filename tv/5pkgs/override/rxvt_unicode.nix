{ fetchurl, rxvt_unicode }:
rxvt_unicode.overrideAttrs (old: {
  patches = old.patches ++ [
    (fetchurl {
      url = https://cgit.krebsco.de/rxvt-unicode/patch/?id=15f3f94;
      sha256 = "12vldwsds27c9l15ffc6svk9mj17jhypcz736pvpmpqbsymlkz2p";
    })
  ];
})
