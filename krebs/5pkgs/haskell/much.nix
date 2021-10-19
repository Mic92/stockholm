{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, blaze-builder, blessings, bytestring, case-insensitive
, containers, data-default, deepseq, directory, either
, email-header, fetchgit, filepath, friendly-time, http-types
, hyphenation, lib, linebreak, network, old-locale
, optparse-applicative, process, random, rosezipper, safe, scanner
, servant-server, split, terminal-size, text, time, transformers
, transformers-compat, unix, vector, wai, warp
}:
mkDerivation {
  pname = "much";
  version = "1.2.0";
  src = fetchgit {
    url = "https://cgit.krebsco.de/much";
    sha256 = "0rf27d7gki5hgivy49xi59ld0j6jw3v7nxi4w1gx6byj1xsarwwl";
    rev = "29749366052a8f6c05b314f1ff17201717855ad7";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson attoparsec base base64-bytestring blaze-builder blessings
    bytestring case-insensitive containers data-default deepseq
    directory either email-header filepath friendly-time http-types
    hyphenation linebreak network old-locale optparse-applicative
    process random rosezipper safe scanner servant-server split
    terminal-size text time transformers transformers-compat unix
    vector wai warp
  ];
  executableHaskellDepends = [
    aeson base blessings bytestring case-insensitive containers
    data-default deepseq directory filepath hyphenation linebreak
    process rosezipper safe scanner text time transformers unix
  ];
  license = lib.licenses.mit;
}
