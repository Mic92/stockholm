{ mkDerivation, aeson, async, attoparsec, base, blessings
, bytestring, containers, data-default, filepath, hashable, lens
, lens-aeson, lib, network, network-simple, network-simple-tls
, network-uri, pcre-light, process, random, servant-server
, string-conversions, stringsearch, text, time, transformers
, unagi-chan, unix, unordered-containers, vector, wai, warp
, fetchgit
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.4.2";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    hash = "sha256-JPQyy0hDSH5JqQGjwoO5BNsD4qk+GKP1VH+j4/2cqes";
    rev = "53a11f421fb18e8687fa06e5511cea8bd9defc36";
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
  license = lib.licenses.mit;
}
