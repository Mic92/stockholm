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
  version = "1.1.0";

  src = fetchgit {
    url = http://cgit.cd.krebsco.de/populate;
    rev = "refs/tags/v${version}";
    sha256 = "19drrzji4zdpfn9r1cfms8j4jrn7xgqpyrs6bfd5zszrx4wyw3gw";
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
