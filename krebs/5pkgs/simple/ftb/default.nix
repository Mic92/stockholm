{ stdenv, fetchurl
, jre, libX11, libXext, libXcursor, libXrandr, libXxf86vm
, openjdk
, mesa_glu, openal
, useAlsa ? false, alsaOss ? null }:
with stdenv.lib;

assert useAlsa -> alsaOss != null;

stdenv.mkDerivation {
  name = "ftb";

  src = fetchurl {
    url = "http://ftb.cursecdn.com/FTB2/launcher/FTB_Launcher.jar";
    sha256 = "0pyh83hhni97ryvz6yy8lyiagjrlx67cwr780s2bja92rxc1sqpj";
  };

  phases = "installPhase";

  installPhase = ''
    set -x
    mkdir -pv $out/bin
    cp -v $src $out/ftb.jar

    cat > $out/bin/ftb << EOF
    #!${stdenv.shell}

    export _JAVA_AWT_WM_NONREPARENTING=1
    export LD_LIBRARY_PATH=\$LD_LIBRARY_PATH:${makeLibraryPath [ libX11 libXext libXcursor libXrandr libXxf86vm mesa_glu openal ]}
    ${if useAlsa then "${alsaOss}/bin/aoss" else "" } \
      ${jre}/bin/java -jar $out/ftb.jar
    EOF

    chmod +x $out/bin/ftb

    ${openjdk}/bin/jar xf $out/ftb.jar favicon.png
  '';
}
