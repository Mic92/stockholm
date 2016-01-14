{pkgs, python3Packages, ...}:

python3Packages.buildPythonPackage rec {
  name = "cacpanel-${version}";
  version = "0.2.3";

  src = pkgs.fetchurl {
    url = "https://pypi.python.org/packages/source/c/cacpanel/cacpanel-${version}.tar.gz";
    sha256 = "1fib7416qqv8yzrj75kxra7ccpz9abqh58b6gkaavws2fa6m3mm8";
  };

  propagatedBuildInputs = with python3Packages; [
    docopt
    requests2
    beautifulsoup4
  ];
}

