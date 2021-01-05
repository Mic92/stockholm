{ mkDerivation, aeson, async, base, bloomfilter, bytestring
, conduit, containers, feed, hslogger, irc-conduit, microlens
, network, optparse-applicative, stdenv, stm, text, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "1.4.7";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "0fkkjvskgaw8dw7vrdp57ry34jpl3bpq92rhnyr9s1nyq0vij328";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson async base bloomfilter bytestring conduit containers feed
    hslogger irc-conduit microlens network optparse-applicative stm
    text wreq
  ];
  license = stdenv.lib.licenses.mit;
}
