{ mkDerivation, aeson, async, attoparsec, base, blessings
, bytestring, containers, data-default, fetchgit, filepath, lens
, lens-aeson, network, network-simple, network-simple-tls
, pcre-heavy, pcre-light, process, random, stdenv, stringsearch
, text, time, transformers, unagi-chan, unix, unordered-containers
, vector
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.1.7";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "1ifjwn5dadlyhbdyym1i3g5vbsc7dyv7qzyprmcvx0qspr3rrk82";
    rev = "0395a9d9815d7d82469f0064738bef80ac87dbe3";
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
