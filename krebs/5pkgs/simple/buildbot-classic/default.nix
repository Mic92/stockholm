{ fetchgit, fetchFromGitHub, python2Packages, ... }:
let
  # https://github.com/NixOS/nixpkgs/issues/14026
  nixpkgs-fix = import (fetchgit {
    url = https://github.com/nixos/nixpkgs;
    rev = "e026b5c243ea39810826e68362718f5d703fb5d0";
    sha256 = "11lqd480bi6xbi7xbh4krrxmbp6a6iafv1d0q3sj461al0x0has8";
  }) {};

in nixpkgs-fix.buildPythonApplication {
  name = "buildbot-classic-0.8.12";
  namePrefix = "";
  patches = [];

  src = fetchFromGitHub {
    owner = "krebscode";
    repo = "buildbot-classic";
    rev = "5b4f5f6f1";
    sha256 = "1j3xn1gjzvsf90jvfmyln71fzlhjx642ivrqf47zfxpkacljja93";
  };
  postUnpack = "sourceRoot=\${sourceRoot}/master";

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
    nixpkgs-fix.pythonPackages.dateutil_1_5
    nixpkgs-fix.pythonPackages.sqlalchemy_migrate_0_7
  ];
  doCheck = false;
  postInstall = ''
    mkdir -p "$out/share/man/man1"
    cp docs/buildbot.1 "$out/share/man/man1"
  '';
}

