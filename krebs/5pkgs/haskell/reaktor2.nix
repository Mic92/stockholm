{ mkDerivation, aeson, attoparsec, base, blessings, bytestring
, containers, fetchgit, filepath, network, network-simple
, network-simple-tls, pcre-heavy, pcre-light, process, random
, stdenv, text, time, transformers, unix, unordered-containers
}:
mkDerivation {
  pname = "reaktor2";
  version = "0.0.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/reaktor2";
    sha256 = "1q2rb78mzpyd8wxfmlbfdz7zq5smsrrvb4n874ap1p8f2bmmp0am";
    rev = "ce276eee82ec0b8c4106beb4c51d6f9eb77335c4";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base blessings bytestring containers filepath
    network network-simple network-simple-tls pcre-heavy pcre-light
    process random text time transformers unix unordered-containers
  ];
  license = stdenv.lib.licenses.mit;
}
