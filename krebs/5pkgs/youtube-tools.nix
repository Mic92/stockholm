{ stdenv, fetchgit, ... }:

stdenv.mkDerivation {
  name = "youtube-tools";

  src = fetchgit {
    url = https://github.com/Lassulus/the_playlist;
    rev = "323a66775168b6addb3acddaee0a8ff227ea4bd4";
    sha256 = "1y1fs2p3xj2yrqpw0h5kd0f3c5p1y70xk1hjnw99sr33r67s9c35";
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
