{ stdenv, fetchgit, xplanet, imagemagick, curl, file }:

stdenv.mkDerivation {
  name = "realwallpaper";

  src = fetchgit {
    url = https://github.com/Lassulus/realwallpaper;
    rev = "e0563289c2ab592b669ce4549fc40130246e9d79";
    sha256 = "1zgk8ips2d686216h203w62wrw7zy9z0lrndx9f8z6f1vpvjcmqc";
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
