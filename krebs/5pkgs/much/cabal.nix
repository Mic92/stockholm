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
  version = "1.1.0";
  src = fetchgit {
    url = "http://cgit.ni.krebsco.de/much";
    sha256 = "1325554zymr1dd0clj8c5ygl70c791csvs0hz33jcfr6b8wysdrl";
    rev = "dfec37d848e11c00d9b7f03295af1fc7b0e83ef5";
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
