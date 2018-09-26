{ pkgs, fetchFromGitHub, python2Packages, git, ... }:

python2Packages.buildPythonApplication rec {
  name = "buildbot-classic-${version}";
  version = "0.8.18";
  namePrefix = "";
  patches = [];

  src = fetchFromGitHub {
    owner = "krebs";
    repo = "buildbot-classic";
    rev = version;
    sha256 = "0b4y3n9zd2gdy8xwk1vpvs4n9fbg72vi8mx4ydgijwngcmdqkjmq";
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

