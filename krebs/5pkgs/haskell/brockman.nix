{ mkDerivation, aeson, aeson-pretty, base, bloomfilter, bytestring
, conduit, containers, directory, feed, filepath, hslogger
, http-client, irc-conduit, lens, network, optparse-applicative
, random, safe, stdenv, text, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "2.1.0";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "1wcv2rmmmnnz6gi3g9l2brqc46wm87byzyrixcnlnx3pj5g4d3zb";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty base bloomfilter bytestring conduit containers
    directory feed filepath hslogger http-client irc-conduit lens
    network optparse-applicative random safe text wreq
  ];
  license = stdenv.lib.licenses.mit;
}
