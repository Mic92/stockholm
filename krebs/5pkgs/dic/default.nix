{ stdenv, fetchgit, coreutils, curl, gnused, gnugrep, ... }:

stdenv.mkDerivation {
  name = "dic";

  src = fetchgit {
    url = https://github.com/krebscode/painload;
    rev = "35ccac73d563ad30d2851b9aeed4cfef69ff74e3";
    sha256 = "0arsfcd8qdrqafw7cxiag7f0kn95nwxxwrs0kdzxxdryfr6l1h07";
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
        < ./util/bin/dic \
        > $out/bin/dic

      chmod +x $out/bin/dic
    '';
}
