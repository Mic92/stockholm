{ mkDerivation, aeson, async, attoparsec, base, blessings
, bytestring, containers, data-default, fetchgit, filepath
, hashable, lens, lens-aeson, network, network-simple
, network-simple-tls, pcre-light, process, random, servant-server
, stdenv, string-conversions, stringsearch, text, time
, transformers, unagi-chan, unix, unordered-containers, vector, wai
, warp
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.2.3-pre1";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "0jg8yln1np5w3bg8jpkymfqi0x3a47pfsk20sz4lp3r2hfpw0zlk";
    rev = "56bb8aef0018650e14624e086bb8884bb7ecd4f7";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async attoparsec base blessings bytestring containers
    data-default filepath hashable lens lens-aeson network
    network-simple network-simple-tls pcre-light process random
    servant-server string-conversions stringsearch text time
    transformers unagi-chan unix unordered-containers vector wai warp
  ];
  license = stdenv.lib.licenses.mit;
}
