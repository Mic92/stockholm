{ mkDerivation, aeson, async, attoparsec, base, blessings
, bytestring, containers, data-default, fetchgit, filepath
, hashable, lens, lens-aeson, network, network-simple
, network-simple-tls, pcre-light, process, random, stdenv
, string-conversions, stringsearch, text, time, transformers
, unagi-chan, unix, unordered-containers, vector
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.2.2";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "1kyr5i5zdzvc7fcyac1i1yvi88kcxafrgp8p79c1b9l4g9sjnv78";
    rev = "9f4e2644188f985d7cd806c13e2c0dee1688b9f0";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async attoparsec base blessings bytestring containers
    data-default filepath hashable lens lens-aeson network
    network-simple network-simple-tls pcre-light process random
    string-conversions stringsearch text time transformers unagi-chan
    unix unordered-containers vector
  ];
  license = stdenv.lib.licenses.mit;
}
