{ stdenv, fetchurl, pythonPackages }:

pythonPackages.buildPythonPackage rec {
  name = "${pname}-${version}";
  pname = "treq";
  version = "15.1.0";
  src = fetchurl {
    url = "mirror://pypi/t/${pname}/${name}.tar.gz";
    sha256= "425a47d5d52a993d51211028fb6ade252e5fbea094e878bb4b644096a7322de8";
  };
  propagatedBuildInputs = with pythonPackages; [
    twisted
    pyopenssl
    requests
    service-identity
  ];
}
