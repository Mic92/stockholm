{ mkDerivation, aeson, aeson-pretty, base, bloomfilter, bytestring
, conduit, containers, directory, feed, filepath, hslogger
, html-entity, http-client, irc-conduit, lens, network
, optparse-applicative, random, safe, stdenv, text, time, timerep
, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "3.2.0";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "0vvps5czl6qcpfyrm2a6vj00hdh941wj4zb2bd9jlgf9mfikqm77";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bloomfilter bytestring conduit containers
    directory feed filepath hslogger html-entity http-client
    irc-conduit lens network optparse-applicative random safe text time
    timerep wreq
  ];
  license = stdenv.lib.licenses.mit;
}
