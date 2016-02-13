{ lib, pythonPackages, fetchurl, ... }:
with pythonPackages; buildPythonPackage rec {
  name = lib.removeSuffix ".tar.gz" (builtins.baseNameOf src.name);

  src = fetchurl {
    url = https://pypi.python.org/packages/source/v/vncdotool/vncdotool-0.9.0.tar.gz;
    sha256 = "1hl44w4x9c8air0g6kp9h1af2vj4lmc50vnsxmca9g080740iyvi";
  };

  propagatedBuildInputs = [
    twisted
    pillow
  ];

  meta = {
    homepage = https://github.com/sibson/vncdotool;
    description = "A command line VNC client and python library";
    license = lib.licenses.mit;
  };
}
