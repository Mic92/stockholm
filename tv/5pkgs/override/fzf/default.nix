self: super:

super.fzf.overrideAttrs (old: {
  patches = old.patches or [] ++ [
    ./complete1.patch
  ];
})
