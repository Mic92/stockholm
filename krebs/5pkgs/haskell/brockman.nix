{ mkDerivation, aeson, aeson-pretty, base, bytestring
, case-insensitive, conduit, containers, directory, feed, filepath
, hslogger, html-entity, http-client, irc-conduit, lens, lrucache
, network, optparse-applicative, random, safe, stdenv, text, time
, timerep, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "3.2.4";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "1jh2i3rxbw8x0p5xs9ph95ixpsa6h6qm0msjb9xqnw9j8by2fkk2";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring case-insensitive conduit
    containers directory feed filepath hslogger html-entity http-client
    irc-conduit lens lrucache network optparse-applicative random safe
    text time timerep wreq
  ];
  license = stdenv.lib.licenses.mit;
}
