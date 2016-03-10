{ stdenv, lib, fetchurl, gtk, glib, libSM, gdk_pixbuf, libX11, libXinerama, iproute,
  makeWrapper, libredirect, ppp, coreutils, gawk, pango }:
stdenv.mkDerivation rec {
  name = "forticlientsslvpn";
  # forticlient will be copied into /tmp before execution. this is necessary as
  # the software demands $base to be writeable

  src = fetchurl {
    # archive.org mirror:
    # https://archive.org/download/ForticlientsslvpnLinux4.4.23171.tar/forticlientsslvpn_linux_4.4.2317.tar.gz
    url = http://www.zen.co.uk/userfiles/knowledgebase/FortigateSSLVPNClient/forticlientsslvpn_linux_4.4.2317.tar.gz;
    sha256 = "19clnf9rgrnwazlpah8zz5kvz6kc8lxawrgmksx25k5ywflmbcrr";
  };
  phases = [ "unpackPhase" "buildPhase" "installPhase" "fixupPhase" ];

  buildInputs = [ makeWrapper ];

  binPath = lib.makeBinPath [
    coreutils
    gawk
  ];


  libPath = lib.makeLibraryPath [
    stdenv.cc.cc
  ];

  guiLibPath = lib.makeLibraryPath [
    gtk
    glib
    libSM
    gdk_pixbuf
    libX11
    libXinerama
    pango
  ];

  buildPhase = ''
    # TODO: 32bit, use the 32bit folder
    patchelf --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
      --set-rpath "$libPath" \
      64bit/forticlientsslvpn_cli

    patchelf --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
      --set-rpath "$libPath:$guiLibPath" \
      64bit/forticlientsslvpn

    patchelf --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
      --set-rpath "$libPath" \
      64bit/helper/subproc

    sed -i 's#\(export PATH=\).*#\1"${binPath}"#' 64bit/helper/waitppp.sh
  '';

  installPhase = ''
    mkdir -p "$out/opt/fortinet"

    cp -r 64bit/. "$out/opt/fortinet"
    wrapProgram $out/opt/fortinet/forticlientsslvpn \
      --set LD_PRELOAD "${libredirect}/lib/libredirect.so" \
      --set NIX_REDIRECTS /usr/sbin/ip=${iproute}/bin/ip:/usr/sbin/ppp=${ppp}/bin/ppp

    mkdir -p "$out/bin/"

    cat > $out/bin/forticlientsslvpn <<EOF
    #!/bin/sh
    # prepare suid bit in tmp
    # TODO maybe tmp does not support suid
    set -euf
    tmpforti=\$(${coreutils}/bin/mktemp -d)
    trap "rm -rf \$tmpforti;" INT TERM EXIT
    cp -r $out/opt/fortinet/. \$tmpforti
    chmod +s \$tmpforti/helper/subproc
    cd \$tmpforti
    "./forticlientsslvpn" "\$@"
    EOF

    chmod +x $out/bin/forticlientsslvpn
    chmod -x $out/opt/fortinet/helper/showlicense
  '';
  meta = {
    homepage = http://www.fortinet.com;
    description = "Forticlient SSL-VPN client";
    license = lib.licenses.unfree;
    maintainers = [ lib.maintainers.makefu ];
  };
}
