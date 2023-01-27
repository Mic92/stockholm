self: super:

let
  prebuilt =
    self.stdenv.mkDerivation rec {
      pname = "iosevka-tv-2";
      version = "15.6.3";
      src = self.fetchurl {
        urls = [
          "https://c.krebsco.de/iosevka-tv-2-${version}.tar.gz"
          "https://ni.krebsco.de/~tv/mirrors/iosevka/iosevka-tv-2-${version}.tar.gz"
        ];
        hash = "sha256-W2bOlM0dYKAt+k2OpNJsNx6/hWEItJ2Tob8thtJJ1BA=";
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
  super.iosevka-tv-2
