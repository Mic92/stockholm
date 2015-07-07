{
  lib.file.url = ../../lib;
  modules.file.url = ../../modules;
  nixpkgs.git = {
    url = https://github.com/NixOS/nixpkgs;
    rev = "e1af50c4c4c0332136283e9231f0a32ac11f2b90";
    cache = ../../tmp/git-cache;
  };
  pubkeys.file.url = ../../pubkeys;
  retiolum-hosts.file.url = ../../hosts;
  secrets.file.url = ../../secrets/nomic/nix;
}
