{ mkDerivation, aeson, async, base, bloomfilter, bytestring
, conduit, containers, feed, hslogger, irc-conduit, microlens
, network, optparse-applicative, stdenv, stm, text, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "1.4.3";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "1721lvdvj47fsif15jvf9x9lsjx0c68fd6i2yjhmhv65cgpivx9q";
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
