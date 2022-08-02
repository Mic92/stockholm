{ pkgs }:

pkgs.stdenv.mkDerivation rec {
  pname = "veroroute";
  version = "2.28";

  src = pkgs.fetchurl {
    url = "mirror://sourceforge/${pname}/${pname}-${version}.tar.gz";
    sha256 = "04dig0g4v1rz50mjj1k6jk99rqbg24hdx8kzrlwv0dlxm567lvc7";
  };

  buildInputs = [
    pkgs.qt5.qtbase
  ];
  nativeBuildInputs = [
    pkgs.qt5.wrapQtAppsHook
  ];

  buildPhase = ''
    qmake Src/veroroute.pro
    make
  '';

  installPhase = ''
    sed -i 's;/usr;;g' veroroute-install.sh
    pkgdir=$out bash ./veroroute-install.sh
  '';
}
