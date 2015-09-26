{ coreutils, gnugrep, gnused, fetchgit, jq, nix, stdenv, ... }:

stdenv.mkDerivation {
  name = "get-1.1.0";

  src = fetchgit {
    url = http://cgit.cd.retiolum/get;
    rev = "e75084e39f0402107bb520b5c9d5434a9d7f5d64";
    sha256 = "5bafc9fa68cdb8ab76437a00354cbe4af4020cbbbbce848c325cae55863d9477";
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
