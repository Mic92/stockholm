{ mkDerivation, ansi-terminal, async, base, binary, bytestring
, data-default, directory, filepath, megaparsec
, optparse-applicative, pandoc, random, safe, scalpel, lib, text
, time
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "recht";
  version = "0.3.0";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "recht";
    rev = version;
    sha256 = "07cyd06wbnzcp33v0nq8cxyggvqrnbni0v2g8cpxar6idn1wlz85";

  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    ansi-terminal async base binary bytestring data-default directory
    filepath megaparsec optparse-applicative pandoc random safe scalpel
    text time
  ];
  license = lib.licenses.mit;
}
