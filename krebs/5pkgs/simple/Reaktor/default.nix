{ lib, pkgs, python3Packages, fetchFromGitHub, ... }:

python3Packages.buildPythonPackage rec {
  name = "Reaktor-${version}";
  version = "0.6.1";

  doCheck = false;

  propagatedBuildInputs = with pkgs;[
    python3Packages.docopt
    python3Packages.requests
  ];
  src = fetchFromGitHub {
    owner = "krebs";
    repo = "Reaktor";
    rev = version;
    sha256 = "0mw2zizv8p264zqqrnb5qyx7szldcza5ma190292a1qlasyg1b4m";
  };
  meta = {
    homepage = http://krebsco.de/;
    description = "An IRC bot based on asynchat";
    license = lib.licenses.wtfpl;
  };
}
