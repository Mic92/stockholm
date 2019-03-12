{ lib, pkgs, python3Packages, fetchFromGitHub, ... }:

python3Packages.buildPythonPackage rec {
  name = "Reaktor-${version}";
  version = "0.7.1";

  doCheck = false;

  propagatedBuildInputs = with pkgs;[
    python3Packages.docopt
    python3Packages.requests
  ];
  src = fetchFromGitHub {
    owner = "krebs";
    repo = "Reaktor";
    rev = "v${version}";
    sha256 = "0cv5a4x73ls6sk8qj2qi6gqn31rv8kvdg13dsf3jv92xdfx6brjn";
  };
  meta = {
    homepage = http://krebsco.de/;
    description = "An IRC bot based on asynchat";
    license = lib.licenses.wtfpl;
  };
}
