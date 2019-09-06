with import <stockholm/lib>;
self: super: {

  exim = super.exim.overrideAttrs (old: rec {
    name = warnOldVersion old.name "exim-4.92.2";
    src = self.fetchurl {
      url = "https://ftp.exim.org/pub/exim/exim4/${name}.tar.xz";
      sha256 = "0m56jsh2fzvwj4rdpcc3pkd5vsi40cjrpzalis7l1zq33m4axmq1";
    };
  });

}
