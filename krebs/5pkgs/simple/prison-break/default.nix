{pkgs, fetchFromGitHub}:
with pkgs.python3.pkgs;

buildPythonPackage rec {
  pname = "prison-break";
  version = "1.0.1";
  src = fetchFromGitHub {
    owner = "makefu";
    repo = pname;
    rev = version;
    sha256 = "1q9bw1hbz0cayclixplyc85kaq05mg6n2zz8mbydljvknidd4p6a";
  };
  propagatedBuildInputs = [
    docopt
    requests
    beautifulsoup4
    (callPackage ./straight-plugin.nix {})
  ];
  checkInputs = [ black ];
}
