{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, blaze-builder, blessings, bytestring, case-insensitive
, containers, deepseq, directory, docopt, email-header, fetchgit
, filepath, friendly-time, hyphenation, linebreak, old-locale
, process, random, rosezipper, safe, scanner, split, stdenv
, terminal-size, text, time, transformers, transformers-compat
, unix, vector
}:
mkDerivation {
  pname = "much";
  version = "1.2.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/much";
    sha256 = "0gfvppi8acylz0q7xh8dkm3dj676d4sc1m1gxwp663bkn4748873";
    rev = "8fc4fbb5bb7781626da8f63cd8df8bb0f554cfe7";
    fetchSubmodules = true;
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson attoparsec base base64-bytestring blaze-builder blessings
    bytestring case-insensitive containers deepseq directory docopt
    email-header filepath friendly-time hyphenation linebreak
    old-locale process random rosezipper safe scanner split
    terminal-size text time transformers transformers-compat unix
    vector
  ];
  license = stdenv.lib.licenses.mit;
}
