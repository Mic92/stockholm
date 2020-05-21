{ stdenv, fetchurl  }:

stdenv.mkDerivation rec {
    pname = "dark-reader-${version}";
    version = "4.8.1";

    extid = "addon@darkreader.org";
    signed = true;

    src = fetchurl {
      url = "https://addons.mozilla.org/firefox/downloads/file/3404143/dark_reader-${version}-an+fx.xpi";
      sha256 = "0ic0i56jhmxymvy68bs5hqcjvdvw3vks5r58i2ygmpsm190rlldb";
    };

    phases = [ "installPhase" ];

    installPhase = ''
      install -D ${src} "$out/${extid}.xpi"
      '';

  meta = with stdenv.lib; {
    description = "Dark mode for every website. Take care of your eyes, use dark theme for night and daily browsing.";
    homepage = https://github.com/darkreader/darkreader;
    license = licenses.mit;
    maintainers = [];
    platforms = stdenv.lib.platforms.all;
  };
}
