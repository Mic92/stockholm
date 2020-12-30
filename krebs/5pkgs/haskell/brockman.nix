{ mkDerivation, aeson, async, base, bloomfilter, bytestring
, conduit, containers, feed, hslogger, irc-conduit, microlens
, network, optparse-applicative, stdenv, stm, text, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "1.4.2";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "0y6ki8kj176knajjfnklrrwwqra23lpz10x5c603rgsjwxgwrxzk";
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
