{ fetchgit, lib, stdenv, ... }:
with builtins;
with lib;
stdenv.mkDerivation rec {
  name = "${baseNameOf src.name}-${removePrefix "refs/tags/v" src.rev}";

  src = fetchgit {
    url = https://github.com/kanaka/noVNC;
    rev = "refs/tags/v0.5.1";
    sha256 = "1vckvvfcawgfqmx415r5rnl6k2alx53jbqmsj49kxpbdvvxpb49d";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    cp -R . $out
  '';
}
