{stdenv,fetchurl,pkgs,python3Packages, ... }:

python3Packages.buildPythonPackage rec {
  name = "drivedroid-gen-repo-${version}";
  version = "0.4.4";

  propagatedBuildInputs = with pkgs;[
    python3Packages.docopt
  ];

  src = fetchurl {
    url = "https://pypi.python.org/packages/source/d/drivedroid-gen-repo/drivedroid-gen-repo-${version}.tar.gz";
    sha256 = "09p58hzp61r5fp025lak9z52y0aakmaqpi59p9w5xq42dvy2hnvl";
  };

  meta = {
    homepage = http://krebsco.de/;
    description = "Generate Drivedroid repos";
    license = stdenv.lib.licenses.wtfpl;
  };
}

