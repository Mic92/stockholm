{ fetchgit, fetchFromGitHub, python2Packages, git, ... }:
let
  # https://github.com/NixOS/nixpkgs/issues/14026
  nixpkgs-fix = import (fetchgit {
    url = https://github.com/nixos/nixpkgs;
    rev = "e026b5c243ea39810826e68362718f5d703fb5d0";
    sha256 = "11lqd480bi6xbi7xbh4krrxmbp6a6iafv1d0q3sj461al0x0has8";
  }) {};

in nixpkgs-fix.buildPythonApplication {
  name = "buildbot-classic-0.8.13";
  namePrefix = "";
  patches = [];

  src = fetchgit {
    url = "https://github.com/krebscode/buildbot-classic";
    rev = "211ec7815";
    sha256 = "09q8wyci7p07lrngjblwnpyxk0wddf8jzabwf598a7yiam6yc4cm";
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

  propagatedBuildInputs = with nixpkgs-fix.pythonPackages; [
    jinja2
    twisted
    dateutil_1_5
    sqlalchemy_migrate_0_7
  ];
  doCheck = false;
  postInstall = ''
    mkdir -p "$out/share/man/man1"
    cp docs/buildbot.1 "$out/share/man/man1"
  '';
}

