{ mkDerivation, attoparsec, base, base64-bytestring, bytestring
, case-insensitive, concise, deepseq, fetchgit, hedgehog, lens, lib
, QuickCheck, quickcheck-instances, random, semigroupoids
, stringsearch, tasty, tasty-golden, tasty-hedgehog, tasty-hunit
, tasty-quickcheck, text, time
}:
mkDerivation {
  pname = "purebred-email";
  version = "0.5.1";
  src = fetchgit {
    url = "https://github.com/purebred-mua/purebred-email";
    sha256 = "0iilyy5dkbzbiazyyfjdz585c3x8b7c2piynmycm7krkc48993vw";
    rev = "7ba346e10ad1521a923bc04a4ffeca479d8dd071";
    fetchSubmodules = true;
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base64-bytestring bytestring case-insensitive
    concise deepseq lens random semigroupoids stringsearch text time
  ];
  testHaskellDepends = [
    attoparsec base bytestring case-insensitive hedgehog lens
    QuickCheck quickcheck-instances random tasty tasty-golden
    tasty-hedgehog tasty-hunit tasty-quickcheck text time
  ];
  homepage = "https://github.com/purebred-mua/purebred-email";
  description = "types and parser for email messages (including MIME)";
  license = lib.licenses.agpl3Plus;
}
