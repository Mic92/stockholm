{ lib, pkgs, pythonPackages, fetchurl, ... }:

with pythonPackages; buildPythonPackage rec {
  name = "bepasty-client-cli-${version}";
  version = "0.3.0";
  propagatedBuildInputs = [
    python_magic
    click
    requests2
  ];

  src = fetchurl {
    url = "https://pypi.python.org/packages/source/b/bepasty-client-cli/bepasty-client-cli-${version}.tar.gz";
    sha256 = "002kcplyfnmr5pn2ywdfilss0rmbm8wcdzz8hzp03ksy2zr4sdbw";
  };

  meta = {
    homepage = https://github.com/bepasty/bepasty-client-cli;
    description = "CLI client for bepasty-server";
    license = lib.licenses.bsd2;
  };
}
