{ mkDerivation, aeson, aeson-pretty, base, bloomfilter, bytestring
, conduit, containers, directory, feed, filepath, hslogger
, http-client, irc-conduit, lens, network, optparse-applicative
, random, safe, stdenv, text, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "2.0.1";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "162pdaxdnrbzy0avdy62id2h5x7477wipqa83ni777fjzx23vi6b";
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
