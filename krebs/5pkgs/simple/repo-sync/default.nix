{ lib, pkgs, python3Packages, fetchurl, ... }:

with python3Packages; buildPythonPackage rec {
  name = "repo-sync-${version}";
  version = "0.2.6";
  disabled = isPy26 || isPy27;
  propagatedBuildInputs = [
    docopt
    GitPython
    pkgs.git
  ];
  src = fetchurl {
    url = "https://pypi.python.org/packages/source/r/repo-sync/repo-sync-${version}.tar.gz";
    sha256 = "1hqa9qw9qg7mxgniqzys9szycs05llg4yik8a9wz94a437zzarsk";
  };
  meta = {
    homepage = http://github.com/makefu/repo-sync;
    description = "Sync remotes to other remotes.";
    license = lib.licenses.mit;
  };
}
