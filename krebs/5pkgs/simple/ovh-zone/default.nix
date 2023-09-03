{ lib, pkgs,python3Packages,fetchurl, ... }:

# TODO: Prepare a diff of future and current
## ovh-zone export krebsco.de --config ~/secrets/krebs/cfg.json |sed 's/[ ]\+/ /g' | sort current
## sed 's/[ ]\+/ /g'/etc/zones/krebsco.de | sort > future
## diff future.sorted current.sorted

python3Packages.buildPythonPackage rec {
  name = "ovh-zone-${version}";
  version = "0.4.4";
  propagatedBuildInputs = with pkgs.python3Packages;[
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
