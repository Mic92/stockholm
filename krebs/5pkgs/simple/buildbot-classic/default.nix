{ pkgs, fetchgit, fetchFromGitHub, python2Packages, git, ... }:

python2Packages.buildPythonApplication {
  name = "buildbot-classic-0.8.13";
  namePrefix = "";
  patches = [];

  src = fetchgit {
    url = "https://github.com/krebscode/buildbot-classic";
    rev = "da5c0204e";
    sha256 = "12aaq8ir9k7n2x9m2jnpcs4rr3pcixncbd3bm36ndh93n80q1z3j";
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

