{ pkgs, ... }:

pkgs.haskellPackages.callPackage (
{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, blaze-builder, blessings, bytestring, case-insensitive, containers, deepseq
, directory, docopt, email-header, fetchgit, filepath
, friendly-time, hyphenation, linebreak, old-locale, process
, random, rosezipper, safe, scanner, split, stdenv, terminal-size, text
, time, transformers, transformers-compat, unix, vector
}:
mkDerivation {
  pname = "much";
  version = "1.0.0";
  src = fetchgit {
    url = "http://cgit.cd.krebsco.de/much";
    rev = "045dc986b4de225a927175f81c8ccfdab450202c";
    sha256 = "cec175e3dc32ef93029ee5285f6c4042ce11d637945bc8cec02cb6699d06cc13";
  };
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec base base64-bytestring blaze-builder blessings bytestring
    case-insensitive containers deepseq directory docopt email-header filepath
    friendly-time hyphenation linebreak old-locale process random rosezipper
    safe scanner split terminal-size text time transformers transformers-compat
    unix vector
  ];
  license = stdenv.lib.licenses.mit;
}
) {}
