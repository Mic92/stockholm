{ mkDerivation, aeson, aeson-pretty, base, bytestring
, case-insensitive, conduit, containers, directory, feed, filepath
, hashable, hslogger, html-entity, http-client, irc-conduit, lens
, lrucache, lrucaching, network, optparse-applicative, random, safe
, stdenv, text, time, timerep, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "3.4.0";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "02nval6a9xcddj6znzxvcb8g6klzjydj1lb4ych64i9mr4a8jvic";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring case-insensitive conduit
    containers directory feed filepath hashable hslogger html-entity
    http-client irc-conduit lens lrucache lrucaching network
    optparse-applicative random safe text time timerep wreq
  ];
  license = stdenv.lib.licenses.mit;
}
