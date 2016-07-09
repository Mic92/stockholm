{ stdenv, fetchurl, python3Packages }:

python3Packages.buildPythonPackage rec {
  name = "urlwatch-2.2";

  src = fetchurl {
    url = "https://thp.io/2008/urlwatch/${name}.tar.gz";
    sha256 = "0s9056mm1hkj5gpzsb5bz6fwxk0nm73i0dhnqwa1bfddjnvpl9d3";
  };

  propagatedBuildInputs = with python3Packages; [
    keyring
    minidb
    pyyaml
    requests2
  ];

  patches = [
    ./setup.patch
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
