{ pkgs, ... }:
pkgs.stdenv.mkDerivation {
  name = "rutorrent-src_2016-12-09";
  src = pkgs.fetchFromGitHub {
    owner = "Novik";
    repo = "rutorrent";
    rev = "580bba8c538b55c1f75f3ad65310ff4ff2a153f7";
    sha256 = "1d9lgrzipy58dnx88z393p152kx6lki0x4aw40k8w9awsci4cx7p";
  };

  phases = [ "installPhase" ];
  installPhase = ''
    cp -r $src $out
  '';
}
