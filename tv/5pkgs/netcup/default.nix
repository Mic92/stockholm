{ coreutils, curl, fetchgit, gawk, gnugrep, gnused, jq, stdenv, w3m, ... }:
with import <stockholm/lib>;
let
  readJSON = path: fromJSON (readFile path);
  sed.escape = replaceChars ["/"] ["\\/"]; # close enough
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
    url = "http://cgit.cd.krebsco.de/netcup";
    rev = "tags/v1.0.0";
    sha256 = "0m6mk16pblvnapxykxdccvphslbv1gjfziyr86bnqin1xb1g99bq";
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
