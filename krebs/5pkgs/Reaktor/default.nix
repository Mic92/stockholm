{ lib, pkgs,python3Packages,fetchurl, ... }:

python3Packages.buildPythonPackage rec {
  name = "Reaktor-${version}";
  version = "0.5.0";
  propagatedBuildInputs = with pkgs;[
    python3Packages.docopt
    python3Packages.requests2
  ];
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/R/Reaktor/Reaktor-${version}.tar.gz";
    sha256 = "1npag52xmnyqv56z0anyf6xf00q0smfzsippal0xdbxrfj7s8qim";
  };
  meta = {
    homepage = http://krebsco.de/;
    description = "An IRC bot based on asynchat";
    license = lib.licenses.wtfpl;
  };
}
