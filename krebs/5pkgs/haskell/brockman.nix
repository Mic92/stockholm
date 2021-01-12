{ mkDerivation, aeson, aeson-pretty, base, bloomfilter, bytestring
, conduit, containers, directory, feed, filepath, hslogger
, html-entity, http-client, irc-conduit, lens, network
, optparse-applicative, random, safe, stdenv, text, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "3.0.0";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "08yla9q2mjd7znpasfwsdqzc3dp2vcvg53x9p4vlx4g7jr3dw3yp";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bloomfilter bytestring conduit containers
    directory feed filepath hslogger html-entity http-client
    irc-conduit lens network optparse-applicative random safe text wreq
  ];
  license = stdenv.lib.licenses.mit;
}
