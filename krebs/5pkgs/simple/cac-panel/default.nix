{pkgs, python3Packages, ...}:

python3Packages.buildPythonPackage rec {
  name = "cac-panel-${version}";
  version = "0.4.4";

  src = pkgs.fetchurl {
    url = "https://pypi.python.org/packages/source/c/cac-panel/cac-panel-${version}.tar.gz";
    sha256 = "16bx67fsbgwxciik42jhdnfzxx1xp5b0rimzrif3r7h4fawlnld8";
  };

  propagatedBuildInputs = with python3Packages; [
    docopt
    requests
    beautifulsoup4
  ];
}

