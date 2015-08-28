{ pkgs, ... }:

let
   hspkgs = pkgs.haskellngPackages.override {
     overrides = self: super: {
       email-header = self.callPackage (
{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, containers, exceptions, fetchgit, QuickCheck
, stdenv, tasty, tasty-quickcheck, text, text-icu, time
}:
mkDerivation {
  pname = "email-header";
  version = "0.3.0";
  src = fetchgit {
    url = "https://github.com/4z3/email-header";
    sha256 = "f33fba567a39b1f2448869b269c26c40d8007599c23ab83bde5b4dfd9fd76ebc";
    rev = "7b179bd31192ead8afe7a0b6e34bcad4039deaa8";
  };
  buildDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    containers exceptions text text-icu time
  ];
  testDepends = [
    base bytestring case-insensitive containers QuickCheck tasty
    tasty-quickcheck text time
  ];
  jailbreak = true;
  homepage = "http://github.com/knrafto/email-header";
  description = "Parsing and rendering of email and MIME headers";
  license = stdenv.lib.licenses.bsd3;
}
) {};
    };
  };
in

hspkgs.callPackage (
{ mkDerivation, aeson, attoparsec, base, base64-bytestring
, blaze-builder, bytestring, case-insensitive, containers, deepseq
, directory, docopt, email-header, fetchgit, filepath
, friendly-time, hyphenation, linebreak, old-locale, process
, random, rosezipper, safe, split, stdenv, terminal-size, text
, time, transformers, transformers-compat, unix, vector
}:
mkDerivation {
  pname = "much";
  version = "0.0.0.0";
  src = fetchgit {
    url = "http://cgit.nomic/much";
    sha256 = "f0bcc34456cb876d3439694d1e16db414a540e13f476fa3ff1ad70d1d3caccb2";
    rev = "bfd854e05207a073eaa983c49f27c37555ccfce5";
  };
  isLibrary = false;
  isExecutable = true;
  buildDepends = [
    aeson attoparsec base base64-bytestring blaze-builder bytestring
    case-insensitive containers deepseq directory docopt email-header
    filepath friendly-time hyphenation linebreak old-locale process
    random rosezipper safe split terminal-size text time transformers
    transformers-compat unix vector
  ];
  license = stdenv.lib.licenses.mit;
}
) {}
