{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let
  mirror = "git@${config.networking.hostName}:";

  defineRepo = name: announce: let
    repo = {
      public = true;
      name = mkDefault "${name}";
      cgit.desc = mkDefault "mirror for ${name}";
      cgit.section = mkDefault "mirror";
      hooks = mkIf announce (mkDefault {
        post-receive = pkgs.git-hooks.irc-announce {
          nick = config.networking.hostName;
          verbose = false;
          channel = "#xxx";
          server = "irc.r";
          branches = [ "master" ];
        };
      });
    };
  in {
    rules = with git; singleton {
      user = with config.krebs.users; [
        config.krebs.users."${config.networking.hostName}-repo-sync"
      ];
      repo = [ repo ];
      perm = push ''refs/*'' [ non-fast-forward create delete merge ];
    };
    repos."${name}" = repo;
  };

  sync-retiolum = name:
    {
      krebs.repo-sync.repos.${name} = {
        branches = {
          makefu = {
            origin.url = "http://cgit.gum/${name}";
            mirror.url = "${mirror}${name}";
          };
          tv = {
            origin.url = "http://cgit.ni.r/${name}";
            mirror.url = "${mirror}${name}";
          };
          nin = {
            origin.url = "http://cgit.onondaga.r/${name}";
            mirror.url = "${mirror}${name}";
          };
          lassulus = {
            origin.url = "http://cgit.lassul.us/${name}";
            mirror.url = "${mirror}${name}";
          };
        };
        latest = {
          url = "${mirror}${name}";
          ref = "heads/master";
        };
      };
      krebs.git = defineRepo name true;
    };

in {
  krebs.repo-sync = {
    enable = true;
  };
  krebs.git = {
    enable = mkDefault true;
    cgit = {
      settings = {
        root-title = "Shared Repos";
        root-desc = "keep on krebsing";
      };
    };
  };
  imports = [
    (sync-retiolum "stockholm")
  ];
}
