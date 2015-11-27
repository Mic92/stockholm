{pkgs, python3Packages, ...}:

python3Packages.buildPythonPackage rec {
  name = "cacpanel-${version}";
  version = "0.2.1";

  src = pkgs.fetchurl {
    url = "https://pypi.python.org/packages/source/c/cacpanel/cacpanel-${version}.tar.gz";
    sha256 = "1zaazg5r10kgva32zh4fhpw6l6h51ijkwpa322na0kh4x6f6aqj3";
  };

  propagatedBuildInputs = with python3Packages; [
    docopt
    requests2
    beautifulsoup4
  ];
}

