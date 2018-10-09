{ boost, fetchurl, file, imagemagick, libudev, libusb, pkgconfig, stdenv
, coreutils, dash, patchelf, writeScriptBin # for add-rpath

, guiSupport ? false, gtkmm2 ? null
, jpegSupport ? true
, networkSupport ? false, dpkg ? null
, ocrSupport ? false, tesseract ? null
, saneSupport ? true, saneBackends ? null
, tiffSupport ? true, libtiff ? null

# Logging defaults copied from Utsushi source (lib/log.cpp)
, logCategory ? "NOTHING"
, logLevel ? "FATAL"
}:

# Logging possibilities copied from Utsushi source (utsushi/log.hpp)
assert builtins.elem logCategory [
  "NOTHING"
  "SANE_BACKEND"
  "ALL"
];
assert builtins.elem logLevel [
  "FATAL" # famous last words
  "ALERT" # outside intervention required
  "ERROR" # something went wrong
  "BRIEF" # short informational notes
  "TRACE" # more chattery feedback
  "DEBUG" # the gory details
  "QUARK" # stack tracing feedback
];

let

  # usage: add-rpath LIBPATH [SOFILE...]
  # Adds LIBPATH to each SOFILE's RPATH
  add-rpath = writeScriptBin "add-rpath" ''
    #! ${dash}/bin/dash
    set -efu
    path=$1; shift
    for file; do
      file=$(${coreutils}/bin/readlink -f "$file")
      old_rpath=$(${patchelf}/bin/patchelf --print-rpath "$file")
      new_rpath=''${old_rpath+$old_rpath:}$path
      ${patchelf}/bin/patchelf --set-rpath "$new_rpath" "$file"
    done
  '';

  imagescan-plugin-networkscan = stdenv.mkDerivation rec {
    name = "imagescan-plugin-networkscan-${meta.version}";

    src =
      if stdenv.system == "i686-linux" then
        fetchurl {
          url = "https://download2.ebz.epson.net/imagescanv3/debian/latest1/deb/x64/imagescan-bundle-debian-9-1.3.21.x86.deb.tar.gz";
          sha256 = "16xv1pdfm2ryis815fawb7zqg6c4swww726g272ssx044r5dp80r";
        }
      else if stdenv.system == "x86_64-linux" then
        fetchurl {
          url = "https://download2.ebz.epson.net/imagescanv3/debian/latest1/deb/x64/imagescan-bundle-debian-9-1.3.21.x64.deb.tar.gz";
          sha256 = "0zik35h2jwrvkwcmq55wc72imidwdnmn1bayhypzhjcz61rasjg2";
        }
      else throw "${name} is not supported on ${stdenv.system} (only i686-linux and x86_64 linux are supported)";

    dontBuild = true;

    installPhase = ''
      # Wildcard * stand for either i386 or amd64
      ${dpkg}/bin/dpkg -x \
          plugins/imagescan-plugin-networkscan_${meta.version}-1epson4debian9_*.deb \
          tmp

      mv tmp/usr $out
    '';

    preFixup = ''
      patchelf --set-interpreter \
          ${stdenv.glibc}/lib/ld-linux${stdenv.lib.optionalString stdenv.is64bit "-x86-64"}.so.2 \
          $out/lib/utsushi/networkscan

      # libstdc++.so.6
      patchelf --set-rpath ${stdenv.cc.cc.lib}/lib \
          $out/lib/utsushi/networkscan
    '';

    meta = {
      description = "Epson Image Scan v3 networkscan plugin";
      longDescription = ''
        This package provides the unfree networkscan plugin from the Epson
        Image Scan v3 scanner driver bundle, which can be used by Utsushi.
      '';
      homepage = "http://support.epson.net/linux/en/imagescanv3.php?version=${meta.version}";
      license = stdenv.lib.licenses.eapl;
      maintainers = [ stdenv.lib.maintainers.tv ];
      platforms = stdenv.lib.platforms.linux;
      version = "1.1.0";
    };
  };

in

stdenv.mkDerivation rec {
  name = "utsushi-${meta.version}";

  src = fetchurl {
    url = "http://support.epson.net/linux/src/scanner/imagescanv3/debian/imagescan_${meta.version}.orig.tar.gz";
    sha256 = "12mzq3wc8gzdma84pjs5gb0gp8mga13wax5g7vjfrzq8pjyqrnmw";
  };

  preConfigure = ''
    substituteInPlace configure \
        --replace /usr/bin/file ${file}/bin/file

    substituteInPlace lib/log.cpp \
        --replace FATAL ${logLevel} \
        --replace NOTHING ${logCategory}
  '';

  postInstall = ''
    # Allow configuration to be done via /etc/utsushi.conf
    ln -s /etc/utsushi.conf $out/etc/utsushi/utsushi.conf

    ${stdenv.lib.optionalString saneSupport ''
      # Make this package compatible with hardware.sane.extraBackends
      mkdir $out/etc/sane.d
      echo utsushi > $out/etc/sane.d/dll.conf
      mkdir $out/lib/sane
      ln -s $out/lib/utsushi/sane/libsane-utsushi.* $out/lib/sane
    ''}

    ${stdenv.lib.optionalString networkSupport ''
      ln -s ${imagescan-plugin-networkscan}/lib/utsushi/networkscan \
        $out/libexec/utsushi/
    ''}
  '';

  # Fixup libraries which otherwise would end up broken like this:
  #
  #  $ ldd .../blah.so | grep libboost_system
  #  libboost_system.so.X.Y.Z => not found
  #  libboost_system.so.X.Y.Z => /nix/store/.../libboost_system.so.X.Y.Z (...)
  #
  preFixup = ''
    add-rpath ${boost}/lib $out/lib/utsushi/libdrv-esci.so
    ${stdenv.lib.optionalString saneSupport ''
      add-rpath ${boost}/lib $out/lib/utsushi/sane/libsane-utsushi.so
    ''}
  '';

  nativeBuildInputs = [
    add-rpath
    pkgconfig
  ];

  buildInputs = [
    boost
    imagemagick
    libudev
    libusb
  ]
  ++ stdenv.lib.optional guiSupport gtkmm2
  ++ stdenv.lib.optional ocrSupport tesseract
  ++ stdenv.lib.optional saneSupport saneBackends
  ++ stdenv.lib.optional tiffSupport libtiff
  ;

  NIX_CFLAGS_COMPILE = [
    "-Wno-error=deprecated-declarations"
    "-Wno-error=unused-variable"
  ];

  configureFlags = [
    "--with-boost=${boost}"
    "--with-magick"
    "--with-magick-pp"
  ]
  ++ stdenv.lib.optionals guiSupport [
    "--with-gtkmm"
  ]
  ++ stdenv.lib.optionals jpegSupport [
    "--with-jpeg"
  ]
  ++ stdenv.lib.optionals saneSupport [
    "--with-sane"
  ]
  ++ stdenv.lib.optionals tiffSupport [
    "--with-tiff"
  ]
  ;

  meta = {
    description = "Utsushi - Next Generation Image Acquisition";
    longDescription = ''
      This software provides applications to easily turn hard-copy
      documents and imagery into formats that are more amenable to
      computer processing.

      Included are a native driver for a number of EPSON scanners
      and a compatibility driver to interface with software built
      around the SANE standard.
    '';
    homepage = http://download.ebz.epson.net/dsc/search/01/search/?OSC=LX;
    license = stdenv.lib.licenses.gpl3;
    maintainers = [ stdenv.lib.maintainers.tv ];
    platforms = stdenv.lib.platforms.linux;
    version = "3.48.0";
  };
}
