{ buildGo116Module , fetchFromGitHub, lib }:

buildGo116Module rec {
  pname = "ergo";
  version = "2.8.0";

  src = fetchFromGitHub {
    owner = "ergochat";
    repo = "ergo";
    rev = "v${version}";
    sha256 = "sha256-xKcSHNH1ksjH1IikqzDi88fOdHR5CHGs8ya4Fj65MbI=";
  };

  vendorSha256 = null;

  meta = {
    description = "A modern IRC server (daemon/ircd) written in Go";
    homepage = "https://github.com/ergochat/ergo";
    license = lib.licenses.mit;
    maintainers = [ lib.maintainers.tv ];
    platforms = lib.platforms.linux;
  };
}
