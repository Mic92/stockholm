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
  version = "1.1.1";

  src = fetchgit {
    url = http://cgit.cd.krebsco.de/populate;
    rev = "refs/tags/v${version}";
    sha256 = "139f4lzn56lca3qgqy9g33r94m3xi1mqns9340lkb4qm6626yvqd";
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
