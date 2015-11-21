{stdenv,fetchurl,pkgs,python3Packages, ... }:

python3Packages.buildPythonPackage rec {
  name = "drivedroid-gen-repo-${version}";
  version = "0.4.2";

  propagatedBuildInputs = with pkgs;[
    python3Packages.docopt
  ];

  src = fetchurl {
    url = "https://pypi.python.org/packages/source/d/drivedroid-gen-repo/drivedroid-gen-repo-${version}.tar.gz";
    sha256 = "1w4dqc9ndyiv5kjh2y8n4p4c280vhqyj8s7y6al2klchcp2ab7q7";
  };

  meta = {
    homepage = http://krebsco.de/;
    description = "Generate Drivedroid repos";
    license = stdenv.lib.licenses.wtfpl;
  };
}

