{ stdenv, fetchgit, coreutils, curl, gnused, gnugrep, ... }:

stdenv.mkDerivation {
  name = "dic";

  src = fetchgit {
    url = http://cgit.ni.krebsco.de/dic;
    rev = "refs/tags/v1.0.2";
    sha256 = "133x2z3dr5synckdvgnyc9fa7jdca43vj0973v148i13x4dqgr36";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase =
    let
      path = stdenv.lib.makeBinPath [
        coreutils
        curl
        gnused
        gnugrep
      ];
    in
    ''
      mkdir -p $out/bin

      sed \
        's,^main() {$,&\n  PATH=${path}; export PATH,' \
        < ./dic \
        > $out/bin/dic

      chmod +x $out/bin/dic
    '';
}
