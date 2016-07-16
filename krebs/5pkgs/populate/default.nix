{ coreutils, fetchgit, jq, openssh, proot, rsync, stdenv, ... }:

let
  PATH = stdenv.lib.makeBinPath [
    coreutils
    jq
    openssh
    proot
    rsync
  ];
in

stdenv.mkDerivation rec {
  name = "populate";
  version = "1.0.0";

  src = fetchgit {
    url = http://cgit.cd.krebsco.de/populate;
    rev = "refs/tags/v${version}";
    sha256 = "12d8p8k6c8096rn423xjnbhbrxacbfpdm7c78g4g5j5nqxknghp5";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    sed \
      '1s,.*,&\nPATH=${PATH},' \
      -i bin/populate

    cp -r . $out
  '';
}
