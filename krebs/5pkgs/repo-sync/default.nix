{ lib, pkgs, python3Packages, fetchurl, ... }:

with python3Packages; buildPythonPackage rec {
  name = "repo-sync-${version}";
  version = "0.2.5";
  disabled = isPy26 || isPy27;
  propagatedBuildInputs = [
    docopt
    GitPython
    pkgs.git
  ];
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/r/repo-sync/repo-sync-${version}.tar.gz";
    sha256 = "1a59bj0vc5ajq8indkvkdk022yzvvv5mjb57hk3xf1j3wpr85p84";
  };
  meta = {
    homepage = http://github.com/makefu/repo-sync;
    description = "Sync remotes to other remotes.";
    license = lib.licenses.mit;
  };
}
