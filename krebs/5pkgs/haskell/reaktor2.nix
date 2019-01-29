{ mkDerivation, aeson, async, attoparsec, base, blessings
, bytestring, containers, data-default, fetchgit, filepath
, hashable, lens, lens-aeson, network, network-simple
, network-simple-tls, pcre-light, process, random, stdenv
, string-conversions, stringsearch, text, time, transformers
, unagi-chan, unix, unordered-containers, vector
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.2.1";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "0wg76wlzfi893rl0lzhfs6bkpdcvwvgl6mpnz6w7r8f7znr4a9vr";
    rev = "0e199f7a357a4c5973e5837ec67699cf224ca69c";
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
