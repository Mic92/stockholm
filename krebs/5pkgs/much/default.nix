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
    sha256 = "17jbw7x82a3bgn1qv5k764f103knrf865dmx48h7192vdh8gz766";
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
