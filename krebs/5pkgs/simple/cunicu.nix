{ lib, pkgs }:

pkgs.buildGo120Module rec {
  pname = "cunicu";
  version = "g${lib.substring 0 7 src.rev}";

  buildInputs = [
    pkgs.libpcap
  ];

  # XXX tries to access https://relay.cunicu.li
  doCheck = false;

  src = pkgs.fetchFromGitHub {
    owner = "stv0g";
    repo = "cunicu";
    rev = "3ed8109bef97a10a438e5658c41823b7f812db8e";
    hash = "sha256-FpOJ6/jmnbpufc+kgKwlLtFhOcc2CTe+FvqeV8WEGMc=";
  };

  vendorHash = "sha256-eAawhJK9K8/7FCQiYMI9XCPePYsCVF045Di7SpRZvL4=";
}
