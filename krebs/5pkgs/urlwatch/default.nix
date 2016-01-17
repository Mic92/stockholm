{ stdenv, fetchurl, python3Packages }:

python3Packages.buildPythonPackage rec {
  name = "urlwatch-2.0";

  src = fetchurl {
    url = "https://thp.io/2008/urlwatch/${name}.tar.gz";
    sha256 = "0j38qzw4jxw41vnnpi6j851hqpv8d6p1cbni6cv8r2vqf5307s3b";
  };

  propagatedBuildInputs = with python3Packages; [
    pyyaml
    keyring
    (python3Packages.buildPythonPackage rec {
      name = "minidb-2.0.1";
      src = fetchurl {
        url = "https://thp.io/2010/minidb/${name}.tar.gz";
        sha256 = "1x958zr9jc26vaqij451qb9m2l7apcpz34ir9fwfjg4fwv24z2dy";
      };
      meta = {
        description = "A simple SQLite3-based store for Python objects";
        homepage = https://thp.io/2010/minidb/;
        license = stdenv.lib.licenses.isc;
        maintainers = [ stdenv.lib.maintainers.tv ];
      };
    })
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
}#
