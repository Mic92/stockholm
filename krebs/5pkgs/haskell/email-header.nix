{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, containers, exceptions, fetchgit, QuickCheck
, stdenv, tasty, tasty-quickcheck, text, text-icu, time
}:
mkDerivation rec {
  pname = "email-header";
  version = "0.4.1-tv1";
  src = fetchgit {
    url = "https://github.com/4z3/email-header";
    rev = "refs/tags/v${version}";
    sha256 = "11xjivpj495r2ss9aqljnpzzycb57cm4sr7yzmf939rzwsd3ib0x";
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
