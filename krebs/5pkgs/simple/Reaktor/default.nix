{ lib, pkgs, python3Packages, fetchFromGitHub, ... }:

python3Packages.buildPythonPackage rec {
  name = "Reaktor-${version}";
  version = "0.6.2";

  doCheck = false;

  propagatedBuildInputs = with pkgs;[
    python3Packages.docopt
    python3Packages.requests
  ];
  src = fetchFromGitHub {
    owner = "krebs";
    repo = "Reaktor";
    rev = version;
    sha256 = "0h8pj0x9b5fnxddwrc0f63rxd3275v5phmjc0fv4kiwlzvbcxj6m";
  };
  meta = {
    homepage = http://krebsco.de/;
    description = "An IRC bot based on asynchat";
    license = lib.licenses.wtfpl;
  };
}
