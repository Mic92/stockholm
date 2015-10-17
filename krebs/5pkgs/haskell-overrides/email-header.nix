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
