{ coreutils, gnugrep, gnused, fetchgit, jq, nix, stdenv, ... }:

stdenv.mkDerivation {
  name = "get-1.4.0";

  src = fetchgit {
    url = http://cgit.cd.krebsco.de/get;
    rev = "08757d47c480c130d69270855c6c0371f6b7d385";
    sha256 = "7c609e2cde7a071bbf62241a7bea60313fdbf076b9f7b3d97226417e13e5ba9d";
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
