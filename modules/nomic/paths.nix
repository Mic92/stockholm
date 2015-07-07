{
  lib.file.url = ../../lib;
  modules.file.url = ../../modules;
  nixpkgs.git = {
    url = https://github.com/NixOS/nixpkgs;
    rev = "4e5e441";
    cache = ../../tmp/git-cache;
  };
  pubkeys.file.url = ../../pubkeys;
  retiolum-hosts.file.url = ../../hosts;
  secrets.file.url = ../../secrets/nomic/nix;
}
