{ lib, pkgs, python3Packages, fetchFromGitHub, ... }:

python3Packages.buildPythonPackage rec {
  name = "Reaktor-${version}";
  version = "0.6.0";

  doCheck = false;

  propagatedBuildInputs = with pkgs;[
    python3Packages.docopt
    python3Packages.requests
  ];
  src = fetchFromGitHub {
    owner = "krebs";
    repo = "Reaktor";
    rev = version;
    sha256 = "0nsnv1rixmlg5wkb74b4f5bycb42b9rp4b14hijh558hbsa1b9am";
  };
  meta = {
    homepage = http://krebsco.de/;
    description = "An IRC bot based on asynchat";
    license = lib.licenses.wtfpl;
  };
}
