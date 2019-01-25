{ mkDerivation, aeson, attoparsec, base, blessings, bytestring
, containers, data-default, fetchgit, filepath, lens, lens-aeson
, network, network-simple, network-simple-tls, pcre-heavy
, pcre-light, process, random, stdenv, stringsearch, text, time
, transformers, unagi-chan, unix, unordered-containers, vector
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.1.4";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "1qfm3vb78r02ma8wdcfbwzmigj6skjl53nmp2z7czjcfjhm1zyq5";
    rev = "147f818a72f4561ed57131e0d181704b599d09f6";
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
