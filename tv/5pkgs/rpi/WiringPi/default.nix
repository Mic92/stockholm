{ fetchFromGitHub, runCommand, stdenv }:

let
  generic = name: extraAttrs:
    stdenv.mkDerivation ({
      pname = "WiringPi-${name}";
      version = "2020-09-14";

      src = fetchFromGitHub (stdenv.lib.importJSON ./src.json);

      buildPhase = ''
        runHook postBuild

        make -C ${name} all

        runHook preBuild
      '';

      installPhase = ''
        runHook preInstall

        export DESTDIR=$out
        export PREFIX=
        export LDCONFIG=true

        make -C ${name} install

        runHook postInstall
      '';
    } // extraAttrs);

  fakeutils = runCommand "fakeutils-1.0" {} /* sh */ ''
    mkdir -p $out/bin
    for name in chown chmod; do
      touch $out/bin/$name
      chmod +x $out/bin/$name
    done
  '';
in

rec {
  wiringPi = generic "wiringPi" {};
  wiringPiDev = generic "devLib" {
    buildInputs = [
      wiringPi
    ];
  };
  gpio = generic "gpio" {
    preInstall = ''
      # fakeutils cannot be buildInputs because they have to override existing
      # executables and therefore need to be prepended to the search path.
      PATH=${fakeutils}/bin:$PATH

      mkdir -p $out/bin
    '';
    buildInputs = [
      wiringPi
      wiringPiDev
    ];
  };
}
