{ lib, pkgs, python3Packages, fetchFromGitHub, ... }:

with python3Packages; buildPythonPackage rec {
  name = "repo-sync-${version}";
  version = "0.2.7";
  disabled = isPy26 || isPy27;
  propagatedBuildInputs = [
    docopt
    GitPython
    pkgs.git
  ];
  src = fetchFromGitHub {
    owner = "krebscode";
    repo = "repo-sync";
    rev = version;
    sha256 = "1qjf1jmxf7xzwskybdys4vqncnwj9f3xwk1gv354zrla68s533cw";
  };
  meta = {
    homepage = http://github.com/makefu/repo-sync;
    description = "Sync remotes to other remotes.";
    license = lib.licenses.mit;
  };
}
