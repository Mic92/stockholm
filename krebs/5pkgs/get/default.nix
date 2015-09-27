{ coreutils, gnugrep, gnused, fetchgit, jq, nix, stdenv, ... }:

stdenv.mkDerivation {
  name = "get-1.1.1";

  src = fetchgit {
    url = http://cgit.cd.retiolum/get;
    rev = "e64826a4f5f74cbaa895e538b97d0e523e9709f9";
    sha256 = "4d1aa07bba52f697cf7aa7ad1b02b9ff41598dfea83c578e77b8d81e3e8830d2";
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
