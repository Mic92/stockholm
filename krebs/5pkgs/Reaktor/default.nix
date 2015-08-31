{ lib, pkgs,python3Packages,fetchurl, ... }:

python3Packages.buildPythonPackage rec {
  name = "Reaktor-${version}";
  version = "0.4.0";
  propagatedBuildInputs = with pkgs;[
    python3Packages.docopt
    python3Packages.requests
  ];
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/R/Reaktor/Reaktor-${version}.tar.gz";
    sha256 = "0izwpq6smp86964hiddnk2bbx8g27nrzfbvdp790bjmnw6gk64nb";
  };
  meta = {
    homepage = http://krebsco.de/;
    description = "An IRC bot based on asynchat";
    license = lib.licenses.wtfpl;
  };
}
