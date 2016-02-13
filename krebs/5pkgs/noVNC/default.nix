{ fetchgit, lib, stdenv, ... }:
with builtins;
with lib;
stdenv.mkDerivation rec {
  name = "${baseNameOf src.name}-${removePrefix "refs/tags/v" src.rev}";

  src = fetchgit {
    url = https://github.com/kanaka/noVNC;
    rev = "refs/tags/v0.5.1";
    sha256 = "1azsnppwnrsv3axj0r5bw8lfarkibgz5vhgcyj8dzn4afn98f55w";
  };

  phases = [
    "unpackPhase"
    "installPhase"
  ];

  installPhase = ''
    cp -R . $out
  '';
}
