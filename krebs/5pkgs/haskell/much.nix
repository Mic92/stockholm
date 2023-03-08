{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, blaze-builder, blessings, bytestring, case-insensitive
, containers, data-default, deepseq, directory, either
, email-header, fetchgit, filepath, friendly-time, http-types
, hyphenation, lib, linebreak, network, old-locale
, optparse-applicative, process, random, rosezipper, safe, scanner
, servant-server, split, terminal-size, text, time, transformers
, transformers-compat, unix, vector, wai, warp
}:
mkDerivation rec {
  pname = "much";
  version = "1.3.2";
  src = fetchgit {
    url = "https://cgit.krebsco.de/much";
    hash = "sha256-q65EYO1d3NYVv2NECkGWPb1TyHGdARNi/GX4pgQmljc=";
    rev = "refs/tags/${version}";
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
