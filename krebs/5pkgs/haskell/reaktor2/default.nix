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
  version = "0.4.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "0bnn23hjl57y0a5rf3h8kq078dziby7il7fandz5wh6s4i3psicp";
    rev = "v${version}";
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
