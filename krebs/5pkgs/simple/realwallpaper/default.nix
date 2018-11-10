{ stdenv, fetchgit, xplanet, imagemagick, curl, file }:

stdenv.mkDerivation {
  name = "realwallpaper";

  src = fetchgit {
    url = https://github.com/Lassulus/realwallpaper;
    rev = "847faebc9b7e87e4bea078e3a2304ec00b4cdfc0";
    sha256 = "10zihkwj9vpshlxw2jk67zbsy8g4i8b1y4jzna9fdcsgn7s12jrr";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  buildInputs = [
  ];

  installPhase = ''
    mkdir -p $out
    cp realwallpaper.sh $out/realwallpaper.sh
  '';
}
