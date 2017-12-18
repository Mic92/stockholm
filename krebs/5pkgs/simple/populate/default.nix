{ coreutils, fetchgit, findutils, git, gnused, jq, openssh, pass, rsync, stdenv
}:

let
  PATH = stdenv.lib.makeBinPath [
    coreutils
    findutils
    git
    gnused
    jq
    openssh
    pass
    rsync
  ];
in

stdenv.mkDerivation rec {
  name = "populate";
  version = "2.1.0";

  src = fetchgit {
    url = http://cgit.ni.krebsco.de/populate;
    rev = "refs/tags/v${version}";
    sha256 = "0cr50y6h6nps0qgpmi01h0z9wzpv2704y5zgx2salk1grkmvcfmh";
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
