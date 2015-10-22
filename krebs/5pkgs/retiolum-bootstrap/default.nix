{ stdenv,lib,fetchurl, ... }:
with lib;
stdenv.mkDerivation rec {
  name = "retiolum-bootstrap";
  version = "4.2.3";


  src = fetchurl {
    url = https://raw.githubusercontent.com/krebscode/painload/master/retiolum/scripts/tinc_setup/new_install.sh;
    sha256 = "03kmil8q2xm3rdm2jxyah7vww84pw6w01d0c3siid9zpn2j7la9s";
  };

  phases = [
    "installPhase"
  ];

  installPhase = ''
    mkdir -p "$out"
    cp -a ${src} $out/retiolum.sh
  '';

  meta = {
    description = "Retiolum boostrap scripts";
    url = https://github.com/krebscode/painload;
    license = licenses.wtfpl;
    platforms = platforms.unix;
    maintainers = with maintainers; [ makefu ];
  };
}
