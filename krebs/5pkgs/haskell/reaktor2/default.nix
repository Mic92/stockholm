{ mkDerivation, aeson, async, attoparsec, base, blessings
, bytestring, containers, data-default, fetchgit, filepath
, hashable, lens, lens-aeson, network, network-simple
, network-simple-tls, network-uri, pcre-light, process, random
, servant-server, stdenv, string-conversions, stringsearch, text
, time, transformers, unagi-chan, unix, unordered-containers
, vector, wai, warp
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.3.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "02hqpq8wcfd6rvi8qk10zy3f3lrzzqnjwqal4cbvksjn3vahz36h";
    rev = "a6893c00f78a8acd0a4bfe7da87ab6889eabcf21";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async attoparsec base blessings bytestring containers
    data-default filepath hashable lens lens-aeson network
    network-simple network-simple-tls network-uri pcre-light process
    random servant-server string-conversions stringsearch text time
    transformers unagi-chan unix unordered-containers vector wai warp
  ];
  license = stdenv.lib.licenses.mit;
}
