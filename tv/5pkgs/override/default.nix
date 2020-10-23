with import <stockholm/lib>;
self: super: {
  input-fonts = super.input-fonts.overrideAttrs (old: rec {
    src = self.fetchurl {
      url = "http://xu.r/~tv/mirrors/input-fonts/Input-Font-2.zip";
      sha256 = "1vvipqcflz4ximy7xpqy9idrdpq3a0c490hp5137r2dq03h865y0";
    };
    outputHash = null;
    outputHashAlgo = null;
    outputHashMode = null;
  });

  nix-prefetch-github =
    self.python3Packages.callPackage ./nix-prefetch-github.nix {};

  rxvt_unicode = self.callPackage ./rxvt_unicode.nix {
    rxvt_unicode = super.rxvt_unicode;
  };
}
