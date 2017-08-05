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
    rev = "5e36c0ee8707dca8a24688d0b17f1047b0bf5bb5";
    sha256 = "13wc0cw5p50npc1skhf22lcdirxp41y08bnhrp10n76l6wg0r4ma";
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

