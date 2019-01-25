with import <stockholm/lib>;
{ mkDerivation, base, fetchgit, stdenv }: let

  cfg = {
    "18.03" = {
      version = "1.1.0";
      sha256 = "1k908zap3694fcxdk4bb29s54b0lhdh557y10ybjskfwnym7szn1";
    };
    "18.09" = {
      version = "2.1.0";
      sha256 = "0wc8v48bb0bkvypc0j6imvnf8xc8572hykk9sgjhzf2w0ggqxv5d";
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
  libraryHaskellDepends = [ base ];
  doHaddock = false;
  # WTFPL is the true license, which is unknown to cabal.
  license = stdenv.lib.licenses.wtfpl;
}
