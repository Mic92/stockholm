{ lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "crx";
  version = "1.0.0";

  src = pkgs.fetchgit {
    url = https://cgit.krebsco.de/crx;
    rev = "refs/tags/v${version}";
    sha256 = "0nrbqw94lb0fzk9991vaqplszqzdij6vbf1kb2hi48a9bzg8h6z3";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    mkdir -p $out/bin

    ${lib.concatStrings
      (lib.mapAttrsToList
        (name: path: /* sh */ ''
          sed \
            's,^set -efu$,&\nPATH=${lib.makeBinPath path}; export PATH,' \
            < ./${name} \
            > $out/bin/${name}
          chmod +x $out/bin/${name}
        '')
        {
          crxid = [
            pkgs.bc
            pkgs.coreutils
            pkgs.gnused
            pkgs.openssl
            pkgs.xxd
          ];
          crxmake = [
            pkgs.coreutils
            pkgs.gnused
            pkgs.openssl
            pkgs.xxd
            pkgs.zip
          ];
        }
      )
    }
  '';
}
