{ mkDerivation, aeson, aeson-pretty, base, bytestring
, case-insensitive, conduit, containers, directory, feed, filepath
, hashable, hslogger, html-entity, http-client, irc-conduit, lens
, lrucache, lrucaching, network, optparse-applicative, random, safe
, lib, text, time, timerep, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "4.0.4";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "sha256-GOEEUjehFgMMf6cNpi0AP/Rz74sTDEcpKRbLD+6YEz0=";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring case-insensitive conduit
    containers directory feed filepath hashable hslogger html-entity
    http-client irc-conduit lens lrucache network
    optparse-applicative random safe text time timerep wreq
  ];
  license = lib.licenses.mit;
}
