{ coreutils, fetchgit, git, buildbot-classic, python2Packages, ... }:

python2Packages.buildPythonApplication {
  name = "buildbot-classic-slave-${buildbot-classic.version}";
  namePrefix = "";

  src = buildbot-classic.src;
  postUnpack = "sourceRoot=\${sourceRoot}/slave";

  nativeBuildInputs = [ git ];
  propagatedBuildInputs = [ python2Packages.twisted ];
  doCheck = false;

  postInstall = ''
    mkdir -p "$out/share/man/man1"
    cp docs/buildslave.1 "$out/share/man/man1"
  '';
}
