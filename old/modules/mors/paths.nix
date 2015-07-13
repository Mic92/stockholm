{
  lib.file.url = ../../lib;
  modules.file.url = ../../modules;
  nixpkgs.git = {
    url = https://github.com/Lassulus/nixpkgs;
    rev = "7ef800430789252dac47f0b67e75a6b9bb616397";
    cache = ../../tmp/git-cache;
  };
  pubkeys.file.url = ../../pubkeys;
  retiolum-hosts.file.url = ../../hosts;
  secrets.file.url = ../../secrets;
}
