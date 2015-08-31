{ lib, pkgs,buildPythonPackage,fetchurl, ... }:

buildPythonPackage rec {
  name = "Reaktor-${version}";
  version = "0.3.5";
  propagatedBuildInputs = with pkgs;[
    pythonPackages.docopt
    pythonPackages.requests
  ];
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/R/Reaktor/Reaktor-${version}.tar.gz";
    sha256 = "1bi92hbm5f4z87biklh8lpjrmfhrdrj7x2hr64lkxpcabgs8kgyh";
  };
  meta = {
    homepage = http://krebsco.de/;
    description = "An IRC bot based on asynchat";
    license = lib.licenses.wtfpl;
  };
}
