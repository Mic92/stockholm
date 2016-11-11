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
  version = "1.2.0";

  src = fetchgit {
    url = http://cgit.ni.krebsco.de/populate;
    rev = "refs/tags/v${version}";
    sha256 = "0q3110hkkxn9bc3a63xbx1hyd1fpzz4wrck4lng3j5a9i1y1jm07";
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
