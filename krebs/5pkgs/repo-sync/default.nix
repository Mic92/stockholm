{ lib, pkgs, python3Packages, fetchurl, ... }:
with python3Packages; buildPythonPackage rec {
  name = "repo-sync-${version}";
  version = "0.2.0";
  disabled = isPy26 || isPy27;
  propagatedBuildInputs = [
    docopt
    GitPython
  ];
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/r/repo-sync/repo-sync-${version}.tar.gz";
    sha256 = "161ssq4138c327p5d68vy91psldal7vp61vk3xdqkhpzd2nz5ag5";
  };
  meta = {
    homepage = http://github.com/makefu/repo-sync;
    description = "Sync remotes to other remotes.";
    license = lib.licenses.mit;
  };
}
