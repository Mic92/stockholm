{ pkgs }:

pkgs.libinput.overrideAttrs (old: {
  patches = old.patches or [] ++ [
    (pkgs.fetchurl {
      name = "libinput-winmax2.patch";
      url = "https://github.com/4z3/libinput/commit/2d0ff41.patch";
      sha256 = "0ipsxzjf98g9w2m163gp49zl14wbxs84s0psdnvk7wfiivgcnm1f";
    })
  ];
})
