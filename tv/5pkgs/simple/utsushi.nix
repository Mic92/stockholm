{ fetchurl, lib, stdenv

, boost, file, imagemagick, libudev, libusb, pkgconfig
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
    pname = "imagescan-plugin-networkscan";
    version = "1.1.3";

    src =
      if stdenv.system == "i686-linux" then
        fetchurl {
          urls = [
            "https://download2.ebz.epson.net/imagescanv3/debian/latest1/deb/x86/imagescan-bundle-debian-9-3.59.2.x86.deb.tar.gz"
            "http://ni.r/~tv/mirrors/epson/imagescan-bundle-debian-9-3.59.2.x86.deb.tar.gz"
          ];
          sha256 = "1whw96kcfj65h2jnk72xgyr9jj05fa07d0xjxpaqb0zwdag3465g";
        }
      else if stdenv.system == "x86_64-linux" then
        fetchurl {
          urls = [
            "https://download2.ebz.epson.net/imagescanv3/debian/latest1/deb/x64/imagescan-bundle-debian-9-3.59.2.x64.deb.tar.gz"
            "http://ni.r/~tv/mirrors/epson/imagescan-bundle-debian-9-3.59.2.x64.deb.tar.gz"
          ];
          sha256 = "0kd6mrs48wwss54gw4v9fm7ss5ma2xpn6gd1pz26cgjvp6n8hknn";
        }
      else throw "${pname} is not supported on ${stdenv.system} (only i686-linux and x86_64 linux are supported)";

    dontBuild = true;

    installPhase = ''
      # Wildcard * stand for either i386 or amd64
      ${dpkg}/bin/dpkg -x \
          plugins/imagescan-plugin-networkscan_${version}-1epson4debian9_*.deb \
          tmp

      mv tmp/usr $out
    '';

    preFixup = ''
      patchelf --set-interpreter \
          ${stdenv.glibc}/lib/ld-linux${lib.optionalString stdenv.is64bit "-x86-64"}.so.2 \
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
      homepage = "http://support.epson.net/linux/en/imagescanv3.php?version=${version}";
      license = lib.licenses.eapl;
      maintainers = [ lib.maintainers.tv ];
      platforms = lib.platforms.linux;
    };
  };

in

stdenv.mkDerivation rec {
  pname = "utsushi";
  version = "3.59.2";

  src = fetchurl {
    urls = [
      "http://support.epson.net/linux/src/scanner/imagescanv3/debian/imagescan_${version}.orig.tar.gz"
      "http://ni.r/~tv/mirrors/epson/imagescan_${version}.orig.tar.gz"
    ];
    sha256 = "1mns10mpyjprkrh2bjcg2nda9iyrnd0pf1did9py84glpapkzrdq";
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

    ${lib.optionalString saneSupport ''
      # Make this package compatible with hardware.sane.extraBackends
      mkdir $out/etc/sane.d
      echo utsushi > $out/etc/sane.d/dll.conf
      mkdir $out/lib/sane
      ln -s $out/lib/utsushi/sane/libsane-utsushi.* $out/lib/sane
    ''}

    ${lib.optionalString networkSupport ''
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
    ${lib.optionalString saneSupport ''
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
  ++ lib.optional guiSupport gtkmm2
  ++ lib.optional ocrSupport tesseract
  ++ lib.optional saneSupport saneBackends
  ++ lib.optional tiffSupport libtiff
  ;

  NIX_CFLAGS_COMPILE = [
    "-Wno-error=deprecated-declarations"
    "-Wno-error=unused-variable"
  ];

  configureFlags = [
    "--with-boost=${boost}"
    "--with-magick"
    "--with-magick-pp"
    "--with-udev-confdir=$(out)/etc/udev"
  ]
  ++ lib.optionals guiSupport [
    "--with-gtkmm"
  ]
  ++ lib.optionals jpegSupport [
    "--with-jpeg"
  ]
  ++ lib.optionals saneSupport [
    "--with-sane"
  ]
  ++ lib.optionals tiffSupport [
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
    license = lib.licenses.gpl3;
    maintainers = [ lib.maintainers.tv ];
    platforms = lib.platforms.linux;
  };
}
