{ fetchurl, lib, pkgs, stdenv }:

stdenv.mkDerivation rec {
  pname = "imagescan-plugin-networkscan";
  version = "1.1.3";

  src =
    if stdenv.system == "x86_64-linux" then
      fetchurl {
        urls = [
          "https://download2.ebz.epson.net/imagescanv3/debian/latest1/deb/x64/imagescan-bundle-debian-10-3.63.0.x64.deb.tar.gz"
          "http://ni.r/~tv/mirrors/epson/imagescan-bundle-debian-10-3.63.0.x64.deb.tar.gz"
        ];
        hash = "sha256:1rbz6mjfinag7c2vnyl7lls3gpn8n91sv0p18ilnbw0vaddssn4j";
      }
    else throw "${pname} is not supported on ${stdenv.system}; supported systems: x86_64-linux";

  dontBuild = true;

  nativeBuildInputs = [
    pkgs.dpkg
  ];

  installPhase = ''
    # Wildcard * stand for either i386 or amd64
    dpkg -x \
        plugins/imagescan-plugin-networkscan_${version}-1epson4debian10_*.deb \
        tmp

    mv tmp/usr $out
  '';

  preFixup = ''
    patchelf --set-interpreter \
        ${pkgs.pkgsi686Linux.glibc}/lib/ld-linux-x86-64.so.2 \
        $out/lib/utsushi/networkscan

    # libstdc++.so.6
    patchelf --set-rpath \
        ${stdenv.cc.cc.lib}/lib \
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
}
