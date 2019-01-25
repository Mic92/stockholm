{ mkDerivation, aeson, attoparsec, base, blessings, bytestring
, containers, data-default, fetchgit, filepath, lens, lens-aeson
, network, network-simple, network-simple-tls, pcre-heavy
, pcre-light, process, random, stdenv, stringsearch, text, time
, transformers, unagi-chan, unix, unordered-containers, vector
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.1.5";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "1n20ms61pp0yncs0y0mx40nj80ivv9qqv4l2ya39rfp21anmwf1s";
    rev = "f8c5b4cfe57cb50503b8333d5d06bd0f99fdecc6";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base blessings bytestring containers data-default
    filepath lens lens-aeson network network-simple network-simple-tls
    pcre-heavy pcre-light process random stringsearch text time
    transformers unagi-chan unix unordered-containers vector
  ];
  license = stdenv.lib.licenses.mit;
}
