{ autoconf, stdenv, fetchFromGitLab, pkgconfig, libxml2, libSM
, shared-mime-info

# Even though imported, this causes:
# (rox:32319): Gtk-WARNING **: 20:16:03.163: Could not find the icon 'text-x-log'. The 'hicolor' theme
# was not found either, perhaps you need to install it.
# You can get a copy from:
# 	http://icon-theme.freedesktop.org/releases
#
# XXX can we use propagatedBuildInputs instead?
, hicolor-icon-theme

, libxslt, docbook_xml_dtd_412, docbook_xsl
, gtk ? gtk2, gtk2 # This is normally in top-level/all-packages.nix
}:

let
  version = "2.11";
in stdenv.mkDerivation rec {
  name = "rox-filer-${version}-tv";

  src = fetchFromGitLab {
    owner = "seirios";
    repo = "rox-filer";
    rev = "14354e21bf94a5f3906238706f6b7ac968fa7fce";
    sha256 = "07fz6ns9g7bh1764agl8myy3b0j7qvlkns6dq1lsd5kcf4yx201c";
  };

  nativeBuildInputs = [
    autoconf
    docbook_xsl
    pkgconfig
    libxslt
  ];

  buildInputs = [ libxml2 gtk shared-mime-info hicolor-icon-theme libSM ];

  #patches = [
  #  <nixpkgs/pkgs/desktops/rox/rox-filer/rox-filer-2.11-in-source-build.patch>
  #];

  # go to the source directory after unpacking the sources
  setSourceRoot = "export sourceRoot=source/ROX-Filer";

  ## patch source with defined patches
  #patchFlags = "-p0";

  # patch the main.c to disable the lookup of the APP_DIR environment variable,
  # which is used to lookup the location for certain images when rox-filer
  # starts; rather override the location with an absolute path to the directory
  # where images are stored to prevent having to use a wrapper, which sets the
  # APP_DIR environment variable prior to starting rox-filer
  preConfigure = ''
    (cd src && autoconf)
    sed -i -e "s:g_strdup(getenv(\"APP_DIR\")):\"$out\":" src/main.c
    mkdir build
    cd build
  '';

  preBuild = ''
    for f in \
      ../src/Docs/Manual.xml \
      ../src/Docs/Manual-fr.xml \
      ../src/Docs/Manual-it.xml ;
    do
      substituteInPlace "$f" \
          --replace \
              /usr/share/sgml/docbook/dtd/xml/4.1.2/docbookx.dtd \
              ${docbook_xml_dtd_412}/xml/dtd/docbook/docbookx.dtd
    done
    make -C ../src/Docs MAN=.. || exit 1
  '';

  configureScript = "../src/configure";

  installPhase = ''
    mkdir -p "$out"
    cd ..
    cp -av Help Messages Options.xml ROX images style.css .DirIcon "$out"

    mkdir -p "$out/share/man/man1"
    cp -av src/rox.1 "$out/share/man/man1"

    # the main executable
    mkdir "$out/bin/"
    cp -v ROX-Filer "$out/bin/rox"

    # mime types
    mkdir -p "$out/ROX/MIME"
    cd "$out/ROX/MIME"
    ln -sv text-x-{diff,patch}.png
    ln -sv application-x-font-{afm,type1}.png
    ln -sv application-xml{,-dtd}.png
    ln -sv application-xml{,-external-parsed-entity}.png
    ln -sv application-{,rdf+}xml.png
    ln -sv application-x{ml,-xbel}.png
    ln -sv application-{x-shell,java}script.png
    ln -sv application-x-{bzip,xz}-compressed-tar.png
    ln -sv application-x-{bzip,lzma}-compressed-tar.png
    ln -sv application-x-{bzip-compressed-tar,lzo}.png
    ln -sv application-x-{bzip,xz}.png
    ln -sv application-x-{gzip,lzma}.png
    ln -sv application-{msword,rtf}.png
  '';

  meta = with stdenv.lib; {
    description = "Fast, lightweight, gtk2 file manager";
    homepage = http://rox.sourceforge.net/desktop;
    license = with licenses; [ gpl2 lgpl2 ];
    platforms = platforms.linux;
    maintainers = [ maintainers.eleanor ];
  };
}
