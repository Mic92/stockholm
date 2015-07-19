{ pkgs, stdenv, ... }:

stdenv.mkDerivation {
  name = "viljetic-pages-0";
  phases = [
    "installPhase"
  ];
  buildInputs = with pkgs; [
    imagemagick
  ];
  installPhase = ''
    mkdir -p $out
    cp ${./index.html} $out/index.html
    convert ${./logo.xpm} $out/favicon2.png
  '';
}
