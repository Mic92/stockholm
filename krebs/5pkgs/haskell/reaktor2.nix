{ mkDerivation, aeson, attoparsec, base, blessings, bytestring
, containers, data-default, fetchgit, filepath, lens, lens-aeson
, network, network-simple, network-simple-tls, pcre-heavy
, pcre-light, process, random, stdenv, text, time, transformers
, unagi-chan, unix, unordered-containers
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.1.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "0g5b40y2gbknghzw12jar1im87k0g4hjg259wf1gp5v55dh3xwk6";
    rev = "d40815fd56bf1895af89b72b1171675a2e0ae5f7";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base blessings bytestring containers data-default
    filepath lens lens-aeson network network-simple network-simple-tls
    pcre-heavy pcre-light process random text time transformers
    unagi-chan unix unordered-containers
  ];
  license = stdenv.lib.licenses.mit;
}
