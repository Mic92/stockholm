{ rxvt_unicode }:
rxvt_unicode.overrideAttrs (old: {
  patches = old.patches ++ [
    ./finish-running-selection.patch
  ];
})
