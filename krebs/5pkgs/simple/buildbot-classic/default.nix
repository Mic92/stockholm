{ pkgs, fetchgit, fetchFromGitHub, python2Packages, git, ... }:

python2Packages.buildPythonApplication rec {
  name = "buildbot-classic-${version}";
  version = "0.8.14";
  namePrefix = "";
  patches = [];

  src = fetchgit {
    url = "https://github.com/krebscode/buildbot-classic";
    rev = "f26147d17";
    sha256 = "096fzcg36qbvfqc3nx3g4608khlkwx81myl1dww1q2i1sa6bgzzh";
    leaveDotGit = true;

  };
  postUnpack = "sourceRoot=\${sourceRoot}/master";
  buildInputs = [ git ];
  patchPhase =
    # The code insists on /usr/bin/tail, /usr/bin/make, etc.
    '' echo "patching erroneous absolute path references..."
       for i in $(find -name \*.py)
       do
         sed -i "$i" \
             -e "s|/usr/bin/python|$(type -P python)|g ; s|/usr/bin/||g"
       done

      sed -i 's/==/>=/' setup.py
    '';

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

