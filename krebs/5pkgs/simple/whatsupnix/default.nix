{ lib, makeWrapper, stdenv
, bash, coreutils, gawk, nix, openssh
}:

stdenv.mkDerivation {
  name = "whatsupnix";
  phases = [ "installPhase" ];
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    mkdir -p $out/bin
    cat - ${./whatsupnix.bash} > $out/bin/whatsupnix <<\EOF
    #! ${bash}/bin/bash
    export PATH=${lib.makeBinPath [ coreutils gawk nix openssh ]}
    EOF
    chmod +x $out/bin/whatsupnix
  '';
}
