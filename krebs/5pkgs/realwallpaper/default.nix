{ stdenv, fetchgit, xplanet, imagemagick, curl, file }:

stdenv.mkDerivation {
  name = "realwallpaper";

  src = fetchgit {
    url = https://github.com/Lassulus/realwallpaper;
    rev = "c2778c3c235fc32edc8115d533a0d0853ab101c5";
    sha256 = "0yhbjz19zk8sj5dsvccm6skkqq2vardn1yi70qmd5li7qvp17mvs";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  buildInputs = [
    xplanet
    imagemagick
    curl
    file
  ];

  installPhase = ''
    mkdir -p $out
    cp realwallpaper.sh $out/realwallpaper.sh
  '';
}
