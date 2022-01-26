{ mkDerivation, aeson, aeson-pretty, base, bytestring
, case-insensitive, conduit, containers, directory, feed, filepath
, hashable, hslogger, html-entity, http-client, irc-conduit, lens
, lrucache, lrucaching, network, optparse-applicative, random, safe
, lib, text, time, timerep, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "4.0.3";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "sha256-rjwroSG9ys0FV2JM70kzmCutMVpUTx8cQ+jQq8Hw1kw=";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bytestring case-insensitive conduit
    containers directory feed filepath hashable hslogger html-entity
    http-client irc-conduit lens lrucache lrucaching network
    optparse-applicative random safe text time timerep wreq
  ];
  license = lib.licenses.mit;
}
