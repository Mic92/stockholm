{ lib, pkgs, python3Packages, fetchurl, ... }:
with python3Packages; buildPythonPackage rec {
  name = "repo-sync-${version}";
  version = "0.1.1";
  disabled = isPy26 || isPy27;
  propagatedBuildInputs = [
    docopt
    GitPython
  ];
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/r/repo-sync/repo-sync-${version}.tar.gz";
    sha256 = "01r30l2bbsld90ps13ip0zi2a41b53dv4q6fxrzvkfrprr64c0vv";
  };
  meta = {
    homepage = http://github.com/makefu/repo-sync;
    description = "Sync remotes to other remotes.";
    license = lib.licenses.mit;
  };
}
