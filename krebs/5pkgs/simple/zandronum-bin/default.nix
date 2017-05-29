{ stdenv
, atk
, bzip2
, cairo
, fetchurl
, fluidsynth
, fontconfig
, freetype
, gdk_pixbuf
, glib
, gtk2
, libjpeg_turbo
, mesa_glu
, mesa_noglu
, openssl
, pango
, SDL
, zlib
, makeWrapper
}:

stdenv.mkDerivation rec {
  name = "zandronum-3.0";

  src = fetchurl {
    url = "http://zandronum.com/downloads/testing/3.0/ZandroDev3.0-170205-2117linux-x86_64.tar.bz2";
    sha256 = "17vrzk0m5b17sp3sqcg57r7812ma97lp3qxn9hmd39fwl1z40fz3";
  };

  libPath = stdenv.lib.makeLibraryPath [
    atk
    bzip2
    cairo
    fluidsynth
    fontconfig
    freetype
    gdk_pixbuf
    glib
    gtk2
    libjpeg_turbo
    mesa_glu
    mesa_noglu
    openssl
    pango
    SDL
    stdenv.cc.cc
    zlib
  ];

  nativeBuildInputs = [ makeWrapper ];

  phases = [ "unpackPhase" "installPhase" ];

  sourceRoot = ".";

  installPhase = ''
    mkdir -p $out/bin
    mkdir -p $out/share/zandronum
    cp *.so *.pk3 zandronum zandronum-server $out/share/zandronum

    patchelf \
      --set-interpreter $(cat ${stdenv.cc}/nix-support/dynamic-linker) \
      --set-rpath $libPath:$out/share/zandronum \
      $out/share/zandronum/zandronum
    patchelf \
      --set-interpreter $(cat ${stdenv.cc}/nix-support/dynamic-linker) \
      --set-rpath $libPath \
      $out/share/zandronum/zandronum-server

    # If we don't set absolute argv0, zandronum.wad file is not found.
    makeWrapper $out/share/zandronum/zandronum $out/bin/zandronum
    makeWrapper $out/share/zandronum/zandronum-server $out/bin/zandronum-server
  '';

  meta = {
    homepage = http://zandronum.com/;
    description = "Multiplayer oriented port, based off Skulltag, for Doom and Doom II by id Software. Binary version for online play";
    maintainers = [ stdenv.lib.maintainers.lassulus ];
    # Binary version has different version string than source code version.
    license = stdenv.lib.licenses.unfreeRedistributable;
    platforms = [ "x86_64-linux" ];
  };
}
