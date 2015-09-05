{ lib, pkgs,python3Packages,fetchurl, ... }:

python3Packages.buildPythonPackage rec {
  name = "krebszones-${version}";
  version = "0.4.3";
  propagatedBuildInputs = with pkgs.python3Packages;[
    d2to1 # for setup to work
    ovh
    docopt
  ];
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/k/krebszones/krebszones-${version}.tar.gz";
    sha256 = "1i6aqy27bikypc4mq7ymfnvf42rr5sxiy6l7gnyk6ifhlp1jq8z5";
  };
  meta = {
    homepage = http://krebsco.de/;
    description = "OVH Zone Upload";
    license = lib.licenses.wtfpl;
  };
}
