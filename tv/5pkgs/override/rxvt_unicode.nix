{ fetchurl, rxvt_unicode }:
rxvt_unicode.overrideAttrs (old: {
  patches = old.patches ++ [
    (fetchurl {
      name = "rxvt-unicode.cancel-running-selection-request.patch";
      url = "https://cgit.krebsco.de/rxvt-unicode/rawdiff/?id=15f3f94&id2=15f3f94^";
      sha256 = "1c7jq8phl85d2f581b4mc6fkmr2nv9n44qjjs4idi51sd2hs7sxw";
    })

    # Fix segfault when calling editor-input from XMonad.
    (fetchurl {
      name = "rxvt-unicode.no-perl_destruct.patch";
      url = "https://cgit.krebsco.de/rxvt-unicode/rawdiff/?id=d63f96a&id2=d63f96a^";
      sha256 = "0fq9w4fq8mw05jk9bblzlh1x51m2cmk62xbc4c1mbiqagjmsi9f8";
    })
  ];
})
