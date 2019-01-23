{ mkDerivation, aeson, attoparsec, base, blessings, bytestring
, containers, data-default, fetchgit, filepath, lens, lens-aeson
, network, network-simple, network-simple-tls, pcre-heavy
, pcre-light, process, random, stdenv, stringsearch, text, time
, transformers, unagi-chan, unix, unordered-containers, vector
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.1.1";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "0d78560gj8hm02zaps63f9mby4lzz82f45i8ypwc9l3lnsypdsr3";
    rev = "4fa5cb937c016f8c10bf8f40d017ca3a436db2d3";
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
