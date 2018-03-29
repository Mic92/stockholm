{ pkgs, fetchFromGitHub, python2Packages, git, ... }:

python2Packages.buildPythonApplication rec {
  name = "buildbot-classic-${version}";
  version = "0.8.16";
  namePrefix = "";
  patches = [];

  src = fetchFromGitHub {
    owner = "krebscode";
    repo = "buildbot-classic";
    rev = version;
    sha256 = "0g686n6m0cjfyympl0ksansllx503gby3hx9gmc8hiyx6x5fkjha";
  };
  postUnpack = "sourceRoot=\${sourceRoot}/master";

  propagatedBuildInputs = [
    python2Packages.jinja2
    python2Packages.twisted
    python2Packages.dateutil_1_5
    python2Packages.sqlalchemy_migrate
    python2Packages.pysqlite
    pkgs.coreutils
  ];
  doCheck = false;
  postInstall = ''
    mkdir -p "$out/share/man/man1"
    cp docs/buildbot.1 "$out/share/man/man1"
  '';
}

