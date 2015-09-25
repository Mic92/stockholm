{ coreutils, gnugrep, gnused, fetchgit, jq, nix, stdenv, ... }:

stdenv.mkDerivation {
  name = "get-1";

  src = fetchgit {
    url = http://cgit.cd.retiolum/get;
    rev = "a39d54aa2e28d8b15a5879024f64f3f41dee9f3b";
    sha256 = "776836e7c5764e547fa46c7d0e14b9a9ccc26c43af288c51096f4073f17b7f32";
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
