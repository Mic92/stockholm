{ autoconf, stdenv, fetchFromGitLab, pkgconfig, libxml2, libSM, shared-mime-info
, libxslt, docbook_xml_dtd_412, docbook_xsl
, gtk ? gtk2, gtk2
}:

stdenv.mkDerivation {
  pname = "rox-filer";
  version = "2.11-tv";

  src = fetchFromGitLab {
    owner = "seirios";
    repo = "rox-filer";
    rev = "3c3ad5d85a1ab548574bf450f730886b60092587";
    sha256 = "0h743zpx1v9rrsaxn0q3nwpq8wkjf6icgzrg8jpqldsphw3ygkhr";
  };

  nativeBuildInputs = [
    autoconf
    docbook_xsl
    libxslt
    pkgconfig
  ];

  buildInputs = [ libxml2 gtk shared-mime-info libSM ];

  # go to the source directory after unpacking the sources
  setSourceRoot = "export sourceRoot=source/ROX-Filer";

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
    homepage = "http://rox.sourceforge.net/desktop";
    license = with licenses; [ gpl2 lgpl2 ];
    platforms = platforms.linux;
    maintainers = [ maintainers.eleanor ];
  };
}
