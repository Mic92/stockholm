{ mkDerivation, aeson, aeson-pretty, base, bloomfilter, bytestring
, conduit, containers, directory, feed, filepath, hslogger
, http-client, irc-conduit, lens, network, optparse-applicative
, random, safe, stdenv, text, wreq
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "brockman";
  version = "1.5.4";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "brockman";
    rev = version;
    sha256 = "1p5bn22sfzgsdmdp14xnsdrbcqd7iy608nz0vgj6zhsabv1bsfdv";
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
