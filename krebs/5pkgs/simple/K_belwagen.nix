{ lib, pkgs, stdenv }:

stdenv.mkDerivation {
  pname = "K_belwagen";
  version = "1.0";

  src = pkgs.painload;
  sourceRoot = "source/K_belwagen";

  buildInputs = [
    pkgs.jack1
    pkgs.pkg-config
  ];

  patchPhase = ''
    sed -i '
      s@^cd@# &@
      s@^make@# &@
      s@^jackd@# &@
      s@^trap@# &@

      s@^set.*@&\nPATH=${lib.makeBinPath [
        pkgs.bc
        pkgs.coreutils
      ]}; export PATH@

      s@\./a\.out@'"$out"'/lib/a.out@
    ' alarm
  '';

  installPhase = ''
    mkdir -p $out/lib
    mkdir -p $out/bin

    cp alarm $out/bin
    cp a.out $out/lib
  '';
}
