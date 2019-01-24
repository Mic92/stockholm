{ mkDerivation, aeson, attoparsec, base, blessings, bytestring
, containers, data-default, fetchgit, filepath, lens, lens-aeson
, network, network-simple, network-simple-tls, pcre-heavy
, pcre-light, process, random, stdenv, stringsearch, text, time
, transformers, unagi-chan, unix, unordered-containers, vector
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.1.3";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "15qhycfja7psnd7v5hn4qb5wrs6bjx4qhny49nkhb7agj4vzwnwi";
    rev = "6c629a0cc422872abdfc40f9621ac0c4f6a420a8";
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
