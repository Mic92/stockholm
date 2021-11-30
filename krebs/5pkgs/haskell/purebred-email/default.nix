{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, concise, deepseq, fetchgit, hedgehog, lens, lib
, QuickCheck, quickcheck-instances, semigroupoids, semigroups
, stringsearch, tasty, tasty-golden, tasty-hedgehog, tasty-hunit
, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "purebred-email";
  version = "0.4.3";
  src = fetchgit {
    url = "https://github.com/purebred-mua/purebred-email";
    sha256 = "06xhccavrdzfsvg65mzdnp0a7b1ilk2rqpnyvkr171ir6mqdpb19";
    rev = "769b360643f699c0a8cd6f1c3a3de36cf0479834";
    fetchSubmodules = true;
  };
  patches = [
    ./untweak-mime-version-header.patch
  ];
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    concise deepseq lens semigroupoids semigroups stringsearch text
    time
  ];
  testHaskellDepends = [
    attoparsec base bytestring case-insensitive hedgehog lens
    QuickCheck quickcheck-instances semigroups tasty tasty-golden
    tasty-hedgehog tasty-hunit tasty-quickcheck text time
  ];
  homepage = "https://github.com/purebred-mua/purebred-email";
  description = "types and parser for email messages (including MIME)";
  license = lib.licenses.agpl3Plus;
}
