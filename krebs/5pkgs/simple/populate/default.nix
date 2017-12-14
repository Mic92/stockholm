{ coreutils, fetchgit, git, gnused, jq, openssh, rsync, stdenv, ... }:

let
  PATH = stdenv.lib.makeBinPath [
    coreutils
    git
    gnused
    jq
    openssh
    rsync
  ];
in

stdenv.mkDerivation rec {
  name = "populate";
  version = "2.0.0";

  src = fetchgit {
    url = http://cgit.ni.krebsco.de/populate;
    rev = "refs/tags/v${version}";
    sha256 = "01cvrg3m2ypg59in1qlr3rd8yzpf002k6pzjls2qb68jwkyf0h2n";
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
