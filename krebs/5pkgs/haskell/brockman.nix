{ mkDerivation, aeson, aeson-pretty, base, bloomfilter, bytestring
, conduit, containers, directory, feed, filepath, hslogger
, http-client, irc-conduit, lens, network, optparse-applicative
, random, safe, stdenv, text, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "2.0.0";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "1c8x674s2y0gakgl2dal2a9q90iaklnk2rgm1vi93jamm4b7w3z7";
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
