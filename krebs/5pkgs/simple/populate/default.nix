{ coreutils, fetchgit, git, jq, openssh, proot, rsync, stdenv, ... }:

let
  PATH = stdenv.lib.makeBinPath [
    coreutils
    git
    jq
    openssh
    proot
    rsync
  ];
in

stdenv.mkDerivation rec {
  name = "populate";
  version = "1.2.4";

  src = fetchgit {
    url = http://cgit.ni.krebsco.de/populate;
    rev = "refs/tags/v${version}";
    sha256 = "0az41vaxfwrh9l19z3cbc7in8pylrnyc0xkzk6773xg2nj4g8a28";
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
