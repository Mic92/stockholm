{ fetchFromGitHub, lib, stdenv, ... }:

stdenv.mkDerivation rec {
  name = "gi";
  version = "master";

  src = fetchFromGitHub {
    owner = "dspinellis";
    repo = "gi";
    rev = "684051e";
    sha256 = "14jgfg0bpzhy7dyq3ff45syd3c324l1z6d3q14izvwk6cs11qs8m";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp ./gi.sh $out/bin/gi
  '';

  meta = {
    description = "a minimalist distributed issue management system based on Git.";
    url = https://github.com/dspinellis/gi;
    license = lib.licenses.gpl3;
    platforms = lib.platforms.unix;
  };
}
