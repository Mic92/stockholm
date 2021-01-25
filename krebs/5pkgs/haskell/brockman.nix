{ mkDerivation, aeson, aeson-pretty, base, bloomfilter, bytestring
, case-insensitive, conduit, containers, directory, feed, filepath
, hslogger, html-entity, http-client, irc-conduit, lens, network
, optparse-applicative, random, safe, stdenv, text, time, timerep
, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "3.2.3";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "1qbjbf0l1ikfzmvky4cnvv7nlcwi2in4afliifh618j0a4f7j427";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bloomfilter bytestring case-insensitive
    conduit containers directory feed filepath hslogger html-entity
    http-client irc-conduit lens network optparse-applicative random
    safe text time timerep wreq
  ];
  license = stdenv.lib.licenses.mit;
}
