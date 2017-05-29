{ mkDerivation, base, bloomfilter, bytestring, feed, fetchgit, lens
, stdenv, wreq
}:
mkDerivation {
  pname = "news";
  version = "1.0.0";
  src = fetchgit {
    url = "http://cgit.lassul.us/news";
    sha256 = "1n3ffr2a5irr5aly0y7qsafag3kxvyyh077ayk0vdwbd0s9hvnjs";
    rev = "c3eb2c0a1a34fc41e18d0bc99b1c4dc73aa6eb20";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base bloomfilter bytestring feed lens wreq
  ];
  license = stdenv.lib.licenses.mit;
}
