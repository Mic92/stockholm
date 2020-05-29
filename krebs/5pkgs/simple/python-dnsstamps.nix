{ python3Packages, lib }:

python3Packages.buildPythonPackage rec {
  pname = "dnsstamps";
  version = "1.3.0";

  src = python3Packages.fetchPypi {
    inherit pname version;
    hash = "sha256:1v334glljw60h9v739jgl8hmyldaawbpv55bbhwq1hcwm5lvdk13";
  };

  postInstall = ''
    mv $out/bin/dnsstamp.py $out/bin/dnsstamp
  '';

  meta = {
    description = "Create and parse DNS stamps with ease";
    homepage = "https://github.com/chrisss404/python-dnsstamps";
    license = lib.licenses.mit;
  };
}
