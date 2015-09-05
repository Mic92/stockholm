{ lib, pkgs,python3Packages,fetchurl, ... }:

python3Packages.buildPythonPackage rec {
  name = "Reaktor-${version}";
  version = "0.4.3";
  propagatedBuildInputs = with pkgs;[
    python3Packages.docopt
    python3Packages.requests2
  ];
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/R/Reaktor/Reaktor-${version}.tar.gz";
    sha256 = "1rvfw9vg7i7z2ah7m5k3zik2b92d3xdaqa8am62qw6vgvmxcmfp4";
  };
  meta = {
    homepage = http://krebsco.de/;
    description = "An IRC bot based on asynchat";
    license = lib.licenses.wtfpl;
  };
}
