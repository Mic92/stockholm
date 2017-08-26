{ lib, pkgs, pythonPackages, fetchFromGitHub, ... }:

with pythonPackages; buildPythonPackage rec {
  name = "bepasty-client-cli";
  propagatedBuildInputs = [
    python_magic
    click
    requests
  ];

  src = fetchFromGitHub {
    owner = "bepasty";
    repo = "bepasty-client-cli";
    rev = "4b7135ba8ba1e17501de08ad7b6aca73c0d949d2";
    sha256 = "1svchyk9zai1vip9ppm12jm7wfjbdr9ijhgcd2n10xh73jrn9cnc";
  };

  meta = {
    homepage = https://github.com/bepasty/bepasty-client-cli;
    description = "CLI client for bepasty-server";
    license = lib.licenses.bsd2;
  };
}
