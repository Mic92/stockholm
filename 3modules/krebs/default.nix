{ config, lib, ... }:

with import ../../4lib/krebs { inherit lib; };
let
  cfg = config.krebs;

  out = {
    imports = [
      ./github-hosts-sync.nix
      ./git.nix
      ./nginx.nix
      ./retiolum.nix
      ./urlwatch.nix
    ];
    options.krebs = api;
    config = mkIf cfg.enable imp;
  };

  api = {
    users = mkOption {
      type = with types; attrsOf user;
      default = addNames {
          lass = {
            pubkey = readFile ../../Zpubkeys/lass.ssh.pub;
          };
          makefu = {
            pubkey = readFile ../../Zpubkeys/makefu.ssh.pub;
          };
          tv = {
            pubkey = readFile ../../Zpubkeys/tv_wu.ssh.pub;
          };
          uriel = {
            pubkey = readFile ../../Zpubkeys/uriel.ssh.pub;
          };
        };
    };
  };

  imp = {
  };

in
out
