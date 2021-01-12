{ mkDerivation, aeson, aeson-pretty, base, bloomfilter, bytestring
, conduit, containers, directory, feed, filepath, hslogger
, http-client, irc-conduit, lens, network, optparse-applicative
, random, safe, stdenv, text, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "2.2.0";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "003crqcqlgai7vwvhvfa7lr5ain8xzs7dm63ksm85mq58cwpsspx";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bloomfilter bytestring conduit containers
    directory feed filepath hslogger http-client irc-conduit lens
    network optparse-applicative random safe text wreq
  ];
  license = stdenv.lib.licenses.mit;
}
