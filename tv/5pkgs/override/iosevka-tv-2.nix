self: super:

let
  srcpkg = super.iosevka-tv-2;
  binpkg = self.fetchzip {
    inherit (srcpkg) pname version;
    stripRoot = false;
    hash = "sha256-PuIrW1ftYD5PW4du6gq1XpUM3v0potwmj+vAxJImF/A=";
    urls = [
      "https://c.krebsco.de/${srcpkg.name}.tar.gz"
      "https://ni.krebsco.de/~tv/mirrors/iosevka/${srcpkg.name}.tar.gz"
    ];
  };
in

if srcpkg.version == binpkg.version then
  binpkg

else
  srcpkg
