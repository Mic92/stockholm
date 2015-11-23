{pkgs, python3Packages, ...}:

python3Packages.buildPythonPackage rec {
  name = "cacpanel-${version}";
  version = "0.2.0";

  src = pkgs.fetchurl {
    url = "https://pypi.python.org/packages/source/c/cacpanel/cacpanel-${version}.tar.gz";
    sha256 = "1rcylbiy6488lpw4s4bildb48fljdq9kn12ksjrl81shmhhq9fcj";
  };

  propagatedBuildInputs = with python3Packages; [
    docopt
    requests2
    beautifulsoup4
  ];
}

