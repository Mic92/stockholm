{ lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  name = "TabFS";

  src = pkgs.fetchgit {
    url = https://cgit.krebsco.de/TabFS;
    rev = "1fc4845283a0e6aa46a8d8978f356d5ccdcedd13";
    sha256 = "0bsm5fhxrr6zwbnm9p10h9pwm85llr02g2ch97a62r62dhjjrc8h";
  };

  phases = [
    "unpackPhase"
    "buildPhase"
    "installPhase"
  ];

  nativeBuildInputs = [
    pkgs.jq
  ];

  buildInputs = [
    pkgs.fuse
  ];

  buildPhase = ''
    make -C fs
  '';

  installPhase = ''
    mkdir -p $out/bin

    cp fs/tabfs $out/bin

    ${lib.concatStrings
      (lib.mapAttrsToList
        (name: spec: /* sh */ ''
          jq < ${spec.source} > $out/bin/${name} \
          --arg out $out \
          --arg path ${lib.makeBinPath spec.path} \
          -Rrs \
          ${lib.escapeShellArg /* jq */ ''
            def when(cond; update): if cond then update else . end;

            split("\n") |
            map(${lib.concatMapStringsSep "|" (filter: "(${filter})")
              (lib.toList (spec.filter or []) ++ [
                /* jq */ ''when(test("^#!"); "\(.)\nexport PATH=\($path)")''
              ])
            }) |
            join("\n")
          ''}
          chmod +x $out/bin/${name}
        '')
        {
          tabfs-enable-native-messaging = {
            source = "install.sh";
            path = [
              pkgs.coreutils
            ];
            filter = /* jq */''
              when(test("^EXE_PATH="); "EXE_PATH=\($out)/bin/tabfs-wrapper")
            '';
          };
          tabfs-wrapper = {
            source = "fs/tabfs-wrapper";
            path = [
              pkgs.coreutils
              pkgs.findutils
              pkgs.gnugrep
              pkgs.procps
              "/run/wrappers" # for fusermount
            ];
          };
          tabfs-build-crx = {
            source = "build-crx.sh";
            path = [
              pkgs.coreutils
              pkgs.crx
              pkgs.gnugrep
              pkgs.jq
              pkgs.openssl
            ];
            filter = /* jq */''
              when(test("^source_dir=");
                sub("\\$\\(dirname \"\\$0\"\\)"; ${builtins.toJSON src})
              )
            '';
          };
          tabfs-install-crx = {
            source = "install-crx.sh";
            path = [
              pkgs.coreutils
            ];
          };
        }
      )
    }
  '';
}
