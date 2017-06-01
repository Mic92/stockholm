{ bash, coreutils, gawk, nix, makeWrapper, stdenv }:

stdenv.mkDerivation {
  name = "whatsupnix";
  phases = [ "installPhase" ];
  nativeBuildInputs = [ makeWrapper ];
  installPhase = ''
    mkdir -p $out/bin
    cat - ${./whatsupnix.bash} > $out/bin/whatsupnix <<\EOF
    #! ${bash}/bin/bash
    export PATH=${stdenv.lib.makeBinPath [ coreutils gawk nix ]}
    EOF
    chmod +x $out/bin/whatsupnix
  '';
}
