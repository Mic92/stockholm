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
        hash = "sha256-88OfNUbuNbGx3hFzYZ+gAYgOWZ+A8IYo45I1n/qOyhM=";
      };
      installPhase = ''
        mkdir $out
        mv * $out/
      '';
    };
in

if super.iosevka.version == prebuilt.version then
  prebuilt

else
  super.iosevka-tv-1
