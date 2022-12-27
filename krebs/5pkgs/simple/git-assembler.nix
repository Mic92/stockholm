{ pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "git-assembler";
  version = "1.3";

  src = pkgs.fetchFromGitLab {
    owner = "wavexx";
    repo = "git-assembler";
    rev = "v${version}";
    hash = "sha256-A+ygt6Fxiu6EkVoQU5L1rhxu2e1HU0nbqJFzLzXzHBo=";
  };

  buildInputs = [
    pkgs.python3
  ];

  buildPhase = ":";

  installPhase = ''
    mkdir -p $out/bin
    cp git-assembler $out/bin
  '';
}
