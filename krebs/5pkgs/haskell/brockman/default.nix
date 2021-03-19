{ mkDerivation, aeson, aeson-pretty, base, bytestring
, case-insensitive, conduit, containers, directory, feed, filepath
, hashable, hslogger, html-entity, http-client, irc-conduit, lens
, lrucache, lrucaching, network, optparse-applicative, random, safe
, stdenv, text, time, timerep, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "3.4.3";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "0s8rvsj5m70wijl8j00fisz6c1kms7kx12lxfgk63mrx2ldp1fb4";
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
