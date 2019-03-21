with import <stockholm/lib>;
self: super: {
  rxvt_unicode = self.callPackage ./rxvt_unicode.nix {
    rxvt_unicode = super.rxvt_unicode;
  };
}
