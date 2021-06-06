{ buildGo116Module , fetchFromGitHub, lib }:

buildGo116Module rec {
  pname = "ergo";
  version = "2.7.0-rc1";

  src = fetchFromGitHub {
    owner = "ergochat";
    repo = "ergo";
    rev = "v${version}";
    sha256 = "0vdrvr991an6f6zsadpsy0npmb4058b278xgc7rh8vhp12m501b4";
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
