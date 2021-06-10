{ pkgs, fetchFromGitHub, python2Packages, git, ... }: let

  # we need the old sqlparse since the new one is python2 incompatible
  sqlparse = python2Packages.callPackage ./sqlparse.nix {};

in python2Packages.buildPythonApplication rec {
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
    python2Packages.dateutil
    (python2Packages.sqlalchemy_migrate.override { sqlparse = sqlparse; })
    python2Packages.pysqlite
    pkgs.coreutils
  ];
  doCheck = false;
  postInstall = ''
    mkdir -p "$out/share/man/man1"
    cp docs/buildbot.1 "$out/share/man/man1"
  '';
}

