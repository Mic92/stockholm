{ coreutils, gnugrep, gnused, fetchgit, jq, nix, stdenv, ... }:

stdenv.mkDerivation {
  name = "get-1.4.1";

  src = fetchgit {
    url = http://cgit.cd.krebsco.de/get;
    rev = "41c0c35805ec1708729f73d14650d8ebc94a405b";
    sha256 = "0rx1qsbb4py14795yhhqwlvaibj2569fqm7x2671l868xi59h9f9";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase =
    let
      path = stdenv.lib.makeBinPath [
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
