{ lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "crx";
  version = "1.1.0";

  src = pkgs.fetchgit {
    url = https://cgit.krebsco.de/crx;
    rev = "refs/tags/v${version}";
    sha256 = "10xwrdxwbvqydayg6a4jcl3cfp4wi9ssm7a0dlnclkc4rmf6sv4a";
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
            pkgs.file
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
