{ stdenv, fetchurl, python3Packages }:

python3Packages.buildPythonPackage rec {
  name = "urlwatch-${meta.version}";

  src = fetchurl {
    url = "https://github.com/thp/urlwatch/archive/${meta.version}.tar.gz";
    sha256 = "09bn31gn03swi7yr3s1ql8x07hx96gap1ka77kk44kk0lvfxn55b";
  };

  propagatedBuildInputs = with python3Packages; [
    keyring
    minidb
    pyyaml
    requests2
  ];

  meta = {
    description = "A tool for monitoring webpages for updates";
    homepage = https://thp.io/2008/urlwatch/;
    license = stdenv.lib.licenses.bsd3;
    maintainers = [ stdenv.lib.maintainers.tv ];
    version = "2.6";
  };
}
