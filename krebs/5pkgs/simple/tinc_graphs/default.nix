{ fetchFromGitHub, lib, pkgs, python3Packages, stdenv }:

python3Packages.buildPythonPackage rec {
  name = "tinc_graphs-${version}";
  version = "0.4.0";

  propagatedBuildInputs = with pkgs;[
    python3Packages.pygeoip
    ## ${geolite-legacy}/share/GeoIP/GeoIPCity.dat
  ];
  src = fetchFromGitHub {
    owner = "makefu";
    repo = "tinc_graphs";
    rev = version;
    sha256 = "0dbnafzz65b1nbgvj7b6skyf4x3f9rrkizmdwpnfh4qgs9ch5xmz";
  };

  preFixup = with pkgs;''
    wrapProgram $out/bin/build-graphs --prefix PATH : "$out/bin"
    wrapProgram $out/bin/all-the-graphs --prefix PATH : "${imagemagick}/bin:${graphviz}/bin:$out/bin"
    wrapProgram $out/bin/tinc-stats2json --prefix PATH : "${tinc}/bin"
  '';

  meta = {
    homepage = http://krebsco.de/;
    description = "Create Graphs from Tinc Stats";
    license = lib.licenses.wtfpl;
  };
}

