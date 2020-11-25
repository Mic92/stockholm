{ fetchurl, rxvt_unicode }:
rxvt_unicode.overrideAttrs (old: {
  patches = old.patches ++ [
    (fetchurl {
      name = "rxvt-unicode.cancel-running-selection-request.patch";
      url = https://cgit.krebsco.de/rxvt-unicode/patch/?id=15f3f94;
      sha256 = "12vldwsds27c9l15ffc6svk9mj17jhypcz736pvpmpqbsymlkz2p";
    })

    # Fix segfault when calling editor-input from XMonad.
    (fetchurl {
      name = "rxvt-unicode.no-perl_destruct.patch";
      url = "https://cgit.krebsco.de/rxvt-unicode/patch/?id=d63f96a";
      sha256 = "0i8nqrqgprv7cygflkrdp5zx75dv9bv84vrr2yc3vnfpqxamc43n";
    })
  ];
})
