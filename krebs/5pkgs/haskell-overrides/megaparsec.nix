{ mkDerivation, base, bytestring, fetchzip, HUnit, mtl, QuickCheck
, stdenv, test-framework, test-framework-hunit
, test-framework-quickcheck2, text, transformers
}:
mkDerivation {
  pname = "megaparsec";
  version = "4.1.0";
  src = fetchzip {
    url = "https://hackage.haskell.org/package/megaparsec-4.1.0/megaparsec-4.1.0.tar.gz";
    sha256 = "1a1ka53a3r91lwnlvzaa8nyk1dxvfd1ij1i5x5vp83q2r9z9dcmi";
  };
  libraryHaskellDepends = [ base bytestring mtl text transformers ];
  testHaskellDepends = [
    base HUnit mtl QuickCheck test-framework test-framework-hunit
    test-framework-quickcheck2 transformers
  ];
  homepage = "https://github.com/mrkkrp/megaparsec";
  description = "Monadic parser combinators";
  license = stdenv.lib.licenses.bsd3;
}
