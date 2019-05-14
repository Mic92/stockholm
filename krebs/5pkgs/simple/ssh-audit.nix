{ fetchFromGitHub, python3Packages, stdenv }:

python3Packages.buildPythonPackage rec {
  inherit (meta) version;

  pname = "ssh-audit";

  src = fetchFromGitHub {
    owner = "arthepsy";
    repo = pname;
    rev = "refs/tags/v${meta.version}";
    sha256 = "0akrychkdym9f6830ysq787c9nc0bkyqvy4h72498lyghwvwc2ms";
  };

  checkInputs = [
    python3Packages.pytest
    python3Packages.pytestcov
  ];

  checkPhase = ''
    py.test --cov-report= --cov=ssh-audit -v test
  '';

  postPatch = ''
    printf %s "$setupPy" > setup.py
    mkdir scripts
    cp ssh-audit.py scripts/ssh-audit
    mkdir ssh-audit
    cp ssh-audit.py ssh-audit/__init__.py
  '';

  setupPy = /* py */ ''
    from distutils.core import setup
    setup(
      author='arthepsy',
      description='${meta.description}',
      license='${meta.license.spdxId}',
      name='${pname}',
      packages=['ssh-audit'],
      scripts=['scripts/ssh-audit'],
      url='${meta.homepage}',
      version='${version}',
    )
  '';

  meta = {
    description = "tool for ssh server auditing";
    homepage = "https://github.com/arthepsy/ssh-audit";
    license = stdenv.lib.licenses.mit;
    maintainers = [
      stdenv.lib.maintainers.tv
    ];
    version = "1.7.0";
  };
}
