{ pkgs, ... }:

let
  hspkgs = pkgs.haskellngPackages.override {
    overrides = self: super: {
      blessings = self.callPackage (
{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "blessings";
  version = "1.0.0";
  src = fetchgit {
    url = http://cgit.cd.retiolum/blessings;
    rev = "25a510dcb38ea9158e9969d56eb66cb1b860ab5f";
    sha256 = "b962153e80e51519b52220199d8350b54154833e4bc25a792ecc58898fef3fb2";
  };
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  # WTFPL is the true license, which is unknown to cabal.
  license = stdenv.lib.licenses.wtfpl;
}
) {};
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
      scanner = self.callPackage (
{ mkDerivation, base, fetchgit, stdenv }:
mkDerivation {
  pname = "scanner";
  version = "1.0.0";
  src = fetchgit {
    url = http://cgit.cd.retiolum/scanner;
    rev = "7f091a3bc152ad3974a1873b460fa1759bf8dcad";
    sha256 = "7d123c227777932039d26fc832b8d32a90f04c0bd6b7e8bcff0a6f49a54e0054";
  };
  libraryHaskellDepends = [ base ];
  license = stdenv.lib.licenses.wtfpl;
}
) {};
    };
  };
in

hspkgs.callPackage (
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
    url = "http://cgit.cd.retiolum/much";
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
