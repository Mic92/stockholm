{ fetchgit, stdenv }:

stdenv.mkDerivation rec {
  name = "nq-${rev}";
  rev = "0eae839cb1";

  src = fetchgit {
    url = https://github.com/chneukirchen/nq;
    inherit rev;
    sha256 = "1150274750cde934932d65bd6053d7a0ba2404a59eadfb87fc6bd8a4fb70febb";
  };

  configurePhase = ''
    sed -i "s:^PREFIX=.*:PREFIX=$out:" Makefile
  '';
}
