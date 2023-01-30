self: super:

let
  srcpkg = super.iosevka-tv-1;
  binpkg = self.fetchzip {
    inherit (srcpkg) pname version;
    stripRoot = false;
    hash = "sha256-QIuTS70vUQSvDDXjY4uI6SCcu1XT4HjvzpthvrNX4h0=";
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
