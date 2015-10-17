{ lib, pkgs,python3Packages,fetchurl, ... }:

python3Packages.buildPythonPackage rec {
  name = "krebszones-${version}";
  version = "0.4.4";
  propagatedBuildInputs = with pkgs.python3Packages;[
    d2to1 # for setup to work
    ovh
    docopt
  ];
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/k/krebszones/krebszones-${version}.tar.gz";
    sha256 = "1bzfc2b9468769j1yj93j12zdlccqbjiqfhql2larximh491sg4d";
  };
  meta = {
    homepage = http://krebsco.de/;
    description = "OVH Zone Upload";
    license = lib.licenses.wtfpl;
  };
}
