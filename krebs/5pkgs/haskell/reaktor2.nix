{ mkDerivation, aeson, attoparsec, base, blessings, bytestring
, containers, data-default, fetchgit, filepath, lens, lens-aeson
, network, network-simple, network-simple-tls, pcre-heavy
, pcre-light, process, random, stdenv, stringsearch, text, time
, transformers, unagi-chan, unix, unordered-containers, vector
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.1.2";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "1198sajpkd6m87j402y796270hwifyn9wk2by6wcrxvwhq1vgs9k";
    rev = "d5f66b27b2cd7c36eb7c2e81b0cdca10c5a5ef90";
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
