{ lib, pkgs, python3Packages, fetchFromGitHub, ... }:

python3Packages.buildPythonPackage rec {
  name = "Reaktor-${version}";
  version = "0.7.0";

  doCheck = false;

  propagatedBuildInputs = with pkgs;[
    python3Packages.docopt
    python3Packages.requests
  ];
  src = fetchFromGitHub {
    owner = "krebs";
    repo = "Reaktor";
    rev = "v${version}";
    sha256 = "12yy06vk0smjs0rmahrn2kd4bcdh1yjw1fz6rifw6nmgx889d9hj";
  };
  meta = {
    homepage = http://krebsco.de/;
    description = "An IRC bot based on asynchat";
    license = lib.licenses.wtfpl;
  };
}
