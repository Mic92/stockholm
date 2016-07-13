{ stdenv, fetchurl, python3Packages }:

python3Packages.buildPythonPackage rec {
  name = "urlwatch-2.5";

  src = fetchurl {
    url = "https://thp.io/2008/urlwatch/${name}.tar.gz";
    sha256 = "0qirpymdmpsx0klmhbx3icmiwpm6fx4wjma646gl9m90pifs8430";
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
