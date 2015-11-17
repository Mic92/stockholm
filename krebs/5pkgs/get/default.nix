{ coreutils, gnugrep, gnused, fetchgit, jq, nix, stdenv, ... }:

stdenv.mkDerivation {
  name = "get-1.3.1";

  src = fetchgit {
    url = http://cgit.cd.krebsco.de/get;
    rev = "64c97edd3f9952cd5e703208c46748a035a515bf";
    sha256 = "32ca83f4fd86fd3285bef9dcfd0917308086d239189858daceca175de49ff97c";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase =
    let
      path = stdenv.lib.makeSearchPath "bin" [
        coreutils
        gnugrep
        gnused
        jq
        nix
      ];
    in
    ''
      mkdir -p $out/bin

      sed \
        '1s,.*,&\nPATH=${path},' \
        < ./get \
        > $out/bin/get

      chmod +x $out/bin/get
    '';
}
