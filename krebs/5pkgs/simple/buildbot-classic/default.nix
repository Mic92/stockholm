{ pkgs, fetchFromGitHub, python2Packages, git, ... }:

python2Packages.buildPythonApplication rec {
  name = "buildbot-classic-${version}";
  version = "0.8.14";
  namePrefix = "";
  patches = [];

  src = fetchFromGitHub {
    owner = "krebscode";
    repo = "buildbot-classic";
    # rev = "v${version}";
    rev = "f40159404";
    sha256 = "0zyjn0bs3vbz89h1vbmn4f27vzl4zkxwnp5kdxnkczdsvpsdycks";
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

