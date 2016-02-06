{ stdenv, fetchurl, python3Packages }:

python3Packages.buildPythonPackage rec {
  name = "urlwatch-2.1";

  src = fetchurl {
    url = "https://thp.io/2008/urlwatch/${name}.tar.gz";
    sha256 = "0xn435cml9wjwk39117p1diqmvw3jbmv9ccr7230iaf7z59vf9v6";
  };

  propagatedBuildInputs = with python3Packages; [
    keyring
    minidb
    pyyaml
    requests2
  ];

  postFixup = ''
    wrapProgram "$out/bin/urlwatch" --prefix "PYTHONPATH" : "$PYTHONPATH"
  '';

  meta = {
    description = "A tool for monitoring webpages for updates";
    homepage = https://thp.io/2008/urlwatch/;
    license = stdenv.lib.licenses.bsd3;
    maintainers = [ stdenv.lib.maintainers.tv ];
  };
}
