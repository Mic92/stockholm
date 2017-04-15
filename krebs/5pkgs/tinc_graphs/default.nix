{stdenv,fetchurl,pkgs,python3Packages, ... }:

python3Packages.buildPythonPackage rec {
  name = "tinc_graphs-${version}";
  version = "0.3.10";
  propagatedBuildInputs = with pkgs;[
    python3Packages.pygeoip
    ## ${geolite-legacy}/share/GeoIP/GeoIPCity.dat
  ];
  src = fetchurl {
    url = "mirror://pypi/t/tinc_graphs/${name}.tar.gz";
    sha256 = "0f4cvb9424fhfmc0hbzmynzh9528fyhx00ayq1nbpgd1p89yw7mc";
  };
  preFixup = with pkgs;''
    wrapProgram $out/bin/build-graphs --prefix PATH : "$out/bin"
    wrapProgram $out/bin/all-the-graphs --prefix PATH : "${imagemagick}/bin:${graphviz}/bin:$out/bin"
    wrapProgram $out/bin/tinc-stats2json --prefix PATH : "${tinc}/bin"
  '';

  meta = {
    homepage = http://krebsco.de/;
    description = "Create Graphs from Tinc Stats";
    license = stdenv.lib.licenses.wtfpl;
  };
}

