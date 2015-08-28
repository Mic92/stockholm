{ stdenv, fetchgit, ... }:

stdenv.mkDerivation {
  name = "youtube-tools";

  src = fetchgit {
    url = https://github.com/Lassulus/the_playlist;
    rev = "9218b163f2d8bc965b853ed9fc9e13d15a703456";
    sha256 = "ae5db4be652d015a518e57e4ed2de34b9127e77d9272af3049832bb134e96e4d";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp bin/* $out/bin/
  '';
}
