{ mkDerivation, aeson, async, attoparsec, base, blessings
, bytestring, containers, data-default, fetchgit, filepath
, hashable, lens, lens-aeson, network, network-simple
, network-simple-tls, network-uri, pcre-light, process, random
, servant-server, lib, string-conversions, stringsearch, text
, time, transformers, unagi-chan, unix, unordered-containers
, vector, wai, warp
}:
mkDerivation rec {
  pname = "reaktor2";
  version = "0.4.0a";
  src = fetchgit {
    url = "https://cgit.lassul.us/reaktor2";
    sha256 = "sha256-x1i2TWcycYVFij6832xaBiQa1RQ1VmSfu5Qt1QrUtds=";
    rev = "6d3eb6de5e770ee26874bb7449934f0c55bd1efa";
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
