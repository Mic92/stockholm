{ mkDerivation, aeson, async, attoparsec, base, blessings
, bytestring, containers, data-default, fetchgit, filepath, lens
, lens-aeson, network, network-simple, network-simple-tls
, pcre-heavy, pcre-light, process, random, stdenv, stringsearch
, text, time, transformers, unagi-chan, unix, unordered-containers
, vector
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.1.6";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "0fkf9g431332fbd4wd496lnbllidyrx96gzwhcjsvaz5m7yla411";
    rev = "ed95e04bb1a5ce6ffc425647bafc7cc50f71f561";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async attoparsec base blessings bytestring containers
    data-default filepath lens lens-aeson network network-simple
    network-simple-tls pcre-heavy pcre-light process random
    stringsearch text time transformers unagi-chan unix
    unordered-containers vector
  ];
  license = stdenv.lib.licenses.mit;
}
