{ fetchgit, lib, stdenv, ... }:

stdenv.mkDerivation rec {
  name = "posix-array-${version}";
  version = "1.0.0";

  src = fetchgit {
    url = https://github.com/makefu/array.git;
    rev = "refs/tags/${version}";
    sha256 = "0yzwlhdg1rgc4p64ny7gj30l7z6vikhskhppsa2qj7s9gm2gz938";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    mkdir -p $out/bin
    cp -a ./array $out/bin
  '';

  meta = {
    description = "POSIX-compliant array implementation";
    url = https://github.com/makefu/array;
    license = lib.licenses.wtfpl;
    platforms = lib.platforms.unix;
    maintainers = with lib.maintainers; [ makefu ];
  };
}
