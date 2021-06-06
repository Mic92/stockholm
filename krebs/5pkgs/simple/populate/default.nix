{ fetchgit, lib, stdenv
, coreutils, findutils, git, gnused, jq, openssh, pass, rsync
}:

let
  PATH = lib.makeBinPath [
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
  version = "2.3.0";

  src = fetchgit {
    url = http://cgit.ni.krebsco.de/populate;
    rev = "refs/tags/v${version}";
    sha256 = "05zr132k1s3a1cc879lvhb83hax7dbfmsbrnxmh7dxjcdg3yhxd7";
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
