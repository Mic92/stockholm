{ fetchFromGitHub, lib, stdenv
, wiringPi ? WiringPi.wiringPi
, wiringPiDev ? WiringPi.wiringPiDev
, WiringPi ? rpiPackages.WiringPi
, rpiPackages
}:

stdenv.mkDerivation {
  pname = "433Utils-RPi_utils";
  version = "2018-06-07";

  src = fetchFromGitHub (lib.importJSON ./src.json);

  patches = [
    ./rc-switch.protocols.patch
    ./RPi_utils.codesend.codestring.patch
  ];

  buildPhase = ''
    runHook postBuild

    make -C RPi_utils

    runHook preBuild
  '';

  buildInputs = [
    wiringPi
    wiringPiDev
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    for name in send codesend RFSniffer; do
      cp RPi_utils/$name $out/bin/
    done

    runHook postInstall
  '';
}
