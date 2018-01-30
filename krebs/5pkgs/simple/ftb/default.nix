{ stdenv, fetchurl, makeDesktopItem
, jre, libX11, libXext, libXcursor, libXrandr, libXxf86vm
, openjdk
, mesa, openal
, useAlsa ? false, alsaOss ? null }:
with stdenv.lib;

assert useAlsa -> alsaOss != null;

let
  desktopItem = makeDesktopItem {
    name = "minecraft";
    exec = "minecraft";
    icon = "minecraft";
    comment = "A sandbox-building game";
    desktopName = "Minecraft";
    genericName = "minecraft";
    categories = "Game;";
  };

in stdenv.mkDerivation {
  name = "ftb";

  src = fetchurl {
    url = "http://ftb.cursecdn.com/FTB2/launcher/FTB_Launcher.jar";
    sha256 = "10ga4jgyfsj5dy4rj2rla0fpnfpnxv8r3bmxpqpwn7fsry4il79v";
  };

  phases = "installPhase";

  installPhase = ''
    set -x
    mkdir -pv $out/bin
    cp -v $src $out/ftb.jar

    cat > $out/bin/ftb << EOF
    #!${stdenv.shell}

    export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:${makeLibraryPath [ libX11 libXext libXcursor libXrandr libXxf86vm mesa openal ]}
    ${if useAlsa then "${alsaOss}/bin/aoss" else "" } \
      ${jre}/bin/java -jar $out/ftb.jar
    EOF

    chmod +x $out/bin/ftb

    mkdir -p $out/share/applications
    ln -s ${desktopItem}/share/applications/* $out/share/applications/

    ${openjdk}/bin/jar xf $out/ftb.jar favicon.png
  '';
}
