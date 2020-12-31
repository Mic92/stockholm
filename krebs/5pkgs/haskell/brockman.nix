{ mkDerivation, aeson, async, base, bloomfilter, bytestring
, conduit, containers, feed, hslogger, irc-conduit, microlens
, network, optparse-applicative, stdenv, stm, text, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "1.4.4";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "12708p3jjrmwdbjrfwlz8v9hfcihav5iwz08gr9qagirqz7qsfyb";
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
