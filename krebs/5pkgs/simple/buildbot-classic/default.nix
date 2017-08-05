{ pkgs, fetchFromGitHub, python2Packages, git, ... }:

python2Packages.buildPythonApplication rec {
  name = "buildbot-classic-${version}";
  version = "0.8.14";
  namePrefix = "";
  patches = [];

  src = fetchFromGitHub {
    owner = "krebscode";
    repo = "buildbot-classic";
    rev = "v${version}";
    sha256 = "0j3mb3g3pgx9nar798igfva7pc5hzcg845gwz8lw7dxr504fky30";
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

