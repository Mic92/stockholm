{ coreutils, curl, fetchgit, gawk, gnugrep, gnused, jq, stdenv, stockholm, w3m, ... }:
with stockholm.lib;

let
  readJSON = path: fromJSON (readFile path);
  sed.escape = replaceStrings ["/"] ["\\/"]; # close enough
  PATH = makeBinPath [
    coreutils
    curl
    gawk
    gnugrep
    gnused
    jq
    w3m
  ];
in
stdenv.mkDerivation {
  name = "netcup-1.0.0";
  src = fetchgit {
    url = "http://cgit.ni.krebsco.de/netcup";
    rev = "refs/tags/v1.0.0";
    sha256 = "1rn7bncfhjw0bqjbvj38m7lks4nyf5qcvkj9dg0zr99ba6dylzx5";
  };
  phases = [ "unpackPhase" "patchPhase" "installPhase" ];
  patchPhase = ''
    path=${shell.escape (sed.escape PATH)}
    sed -i "1s/.*/&\nPATH=$path/" vcp
  '';
  installPhase = ''
    mkdir -p $out/bin
    cp vcp $out/bin
  '';
}
