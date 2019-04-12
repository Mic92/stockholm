with import <stockholm/lib>;
{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, containers, exceptions, fetchgit, QuickCheck
, stdenv, tasty, tasty-quickcheck, text, text-icu, time
}: let

  cfg = {
    "18.03" = {
      version = "0.3.0";
      rev = "7b179bd31192ead8afe7a0b6e34bcad4039deaa8";
      sha256 = "12j2n3sbvzjnw99gga7kkdygm8n3qx2lh8q26ad6a53xm5whnz59";
    };
    "18.09" = {
      version = "0.4.1-tv1";
      rev = "refs/tags/v${cfg.version}";
      sha256 = "11xjivpj495r2ss9aqljnpzzycb57cm4sr7yzmf939rzwsd3ib0x";
    };
    "19.03" = {
      version = "0.4.1-tv1";
      rev = "refs/tags/v${cfg.version}";
      sha256 = "11xjivpj495r2ss9aqljnpzzycb57cm4sr7yzmf939rzwsd3ib0x";
    };
  }.${versions.majorMinor nixpkgsVersion};

in mkDerivation {
  pname = "email-header";
  version = cfg.version;
  src = fetchgit {
    url = "https://github.com/4z3/email-header";
    rev = cfg.rev;
    sha256 = cfg.sha256;
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
