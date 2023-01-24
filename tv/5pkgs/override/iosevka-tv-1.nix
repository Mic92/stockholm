self: super:

let
  prebuilt =
    self.stdenv.mkDerivation rec {
      pname = "iosevka-tv-1";
      version = "15.6.3";
      src = self.fetchurl {
        urls = [
          "https://c.krebsco.de/iosevka-tv-1-${version}.tar.gz"
          "https://ni.krebsco.de/~tv/mirrors/iosevka/iosevka-tv-1-${version}.tar.gz"
        ];
        sha256 = "0shsvlb1cmn39l33nzd5bj8g0h9jg7rdq8hvlac8qvnzfhpi5s6k";
      };
      installPhase = ''
        mkdir -p $out/share/fonts/truetype
        mv -v *.ttf $out/share/fonts/truetype
      '';
    };
in

if super.iosevka.version == prebuilt.version then
  prebuilt

else
  super.iosevka
