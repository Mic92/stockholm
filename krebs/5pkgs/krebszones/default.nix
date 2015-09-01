{ lib, pkgs,python3Packages,fetchurl, ... }:

python3Packages.buildPythonPackage rec {
  name = "krebszones-${version}";
  version = "0.4.2";
  propagatedBuildInputs = with pkgs;[
    python3Packages.d2to1 # for setup to work
    python3Packages.ovh
    python3Packages.docopt
  ];
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/k/krebszones/krebszones-${version}.tar.gz";
    sha256 = "0mmz2camqcmv8pppwbzd2v986v07620dg6p5d9v094v8ij1bdlfk";
  };
  meta = {
    homepage = http://krebsco.de/;
    description = "OVH Zone Upload";
    license = lib.licenses.wtfpl;
  };
}
