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
  version = "1.3.1";
  src = fetchgit {
    url = "https://cgit.krebsco.de/much";
    sha256 = "0gwyhqcvg9ywna8fhb9hnx97qh5inglj3l0pcwkgwcvm27mfpcqa";
    rev = "77357335a3a88a4b93f91a46ab939a1a9b192977";
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
