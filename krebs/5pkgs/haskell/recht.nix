{ mkDerivation, async, base, blessings, data-default, directory
, filepath, optparse-generic, pandoc, random, regex-tdfa, safe
, scalpel, stdenv, text
, fetchFromGitHub
}:
mkDerivation rec {
  pname = "recht";
  version = "0.1.0";
  src = fetchFromGitHub {
    owner = "kmein";
    repo = "recht";
    rev = "e3ed36e969cca138e6fc8199b0234d4fe36b663d";
    sha256 = "1cbdahjrhcx9jwmkncal04ss6rb2bf1ikyfxwvy6ngazfmj1d9f2";

  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    async base blessings data-default directory filepath
    optparse-generic pandoc random regex-tdfa safe scalpel text
  ];
  license = "unknown";
  hydraPlatforms = stdenv.lib.platforms.none;
}
