{ mkDerivation, aeson, async, attoparsec, base, blessings
, bytestring, containers, data-default, fetchgit, filepath
, hashable, lens, lens-aeson, network, network-simple
, network-simple-tls, pcre-light, process, random, stdenv
, string-conversions, stringsearch, text, time, transformers
, unagi-chan, unix, unordered-containers, vector
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.2.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "1wls61d9z9zkvvfgq60clcph0800kpvymqw63dpsk0sp13zygpg9";
    rev = "e9ca12a945b1d1c068e9c31050e264cb20690db4";
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
