{ stdenv, fetchgit, xplanet, imagemagick, curl, file }:

stdenv.mkDerivation {
  name = "realwallpaper";

  src = fetchgit {
    url = https://github.com/Lassulus/realwallpaper;
    rev = "b8408cfb295b6ce5b965309b30358ca6c6409efd";
    sha256 = "0yyl8hhqshw9bx04xs8glvir3c0qzvfrwzmbvyg318mnz5xalcl0";
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
