{ coreutils, fetchgit, git, buildbot-classic, python2Packages, ... }:

python2Packages.buildPythonApplication {
  name = "buildbot-classic-slave-0.8.13";
  namePrefix = "";

  src = buildbot-classic.src;
  postUnpack = "sourceRoot=\${sourceRoot}/slave";

  patchPhase = ":";

  nativeBuildInputs = [ git ];
  propagatedBuildInputs = [ python2Packages.twisted ];
  doCheck = false;

  postInstall = ''
    mkdir -p "$out/share/man/man1"
    cp docs/buildslave.1 "$out/share/man/man1"
  '';
}
