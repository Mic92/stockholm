with import <stockholm/lib>;
{ mkDerivation, base, fetchgit, hspec, QuickCheck, stdenv, text }: let

  cfg = {
    "18.03" = {
      version = "1.1.0";
      sha256 = "1k908zap3694fcxdk4bb29s54b0lhdh557y10ybjskfwnym7szn1";
    };
    "18.09" = {
      version = "2.2.0";
      sha256 = "1pb56dgf3jj2kq3cbbppwzyg3ccgqy9xara62hkjwyxzdx20clk1";
    };
    "19.03" = {
      version = "2.2.0";
      sha256 = "1pb56dgf3jj2kq3cbbppwzyg3ccgqy9xara62hkjwyxzdx20clk1";
    };
  }.${versions.majorMinor nixpkgsVersion};

in mkDerivation {
  pname = "blessings";
  version = cfg.version;
  src = fetchgit {
    url = http://cgit.ni.krebsco.de/blessings;
    rev = "refs/tags/v${cfg.version}";
    sha256 = cfg.sha256;
  };
  libraryHaskellDepends = [ base text ];
  testHaskellDepends = [ base hspec QuickCheck ];
  doHaddock = false;
  # WTFPL is the true license, which is unknown to cabal.
  license = stdenv.lib.licenses.wtfpl;
}
