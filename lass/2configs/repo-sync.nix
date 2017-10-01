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
          channel = "#krebs";
          server = "irc.r";
          branches = [ "newest" ];
        };
      });
    };
  in {
    rules = with git; singleton {
      user = with config.krebs.users; [
        config.krebs.users."${config.networking.hostName}-repo-sync"
        lass
        lass-shodan
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
          ref = "heads/newest";
        };
      };
      krebs.git = defineRepo name (config.networking.hostName == "prism");
    };

  sync-remote = name: url:
    {
      krebs.repo-sync.repos.${name} = {
        branches = {
          remote = {
            origin.url = url;
            mirror.url = "${mirror}${name}";
          };
        };
      };
      krebs.git = defineRepo name (config.networking.hostName == "prism");
    };

  sync-remote-silent = name: url:
    {
      krebs.repo-sync.repos.${name} = {
        branches = {
          remote = {
            origin.url = url;
            mirror.url = "${mirror}${name}";
          };
        };
      };
      krebs.git = defineRepo name false;
    };

in {
  krebs.repo-sync = {
    enable = true;
    unitConfig.ConditionPathExists = "!/var/run/ppp0.pid";
  };
  imports = [
    (sync-remote "array" "https://github.com/makefu/array")
    (sync-remote "email-header" "https://github.com/4z3/email-header")
    (sync-remote "mycube-flask" "https://github.com/makefu/mycube-flask")
    (sync-remote "reaktor-titlebot" "https://github.com/makefu/reaktor-titlebot")
    (sync-remote "repo-sync" "https://github.com/makefu/repo-sync")
    (sync-remote "skytraq-datalogger" "https://github.com/makefu/skytraq-datalogger")
    (sync-remote "xintmap" "https://github.com/4z3/xintmap")
    (sync-remote "realwallpaper" "https://github.com/lassulus/realwallpaper")
    (sync-remote "lassulus-blog" "https://github.com/lassulus/lassulus-blog")
    (sync-remote "painload" "https://github.com/krebscode/painload")
    (sync-remote "Reaktor" "https://github.com/krebscode/Reaktor")
    (sync-remote "nixos-wiki" "https://github.com/Mic92/nixos-wiki.wiki.git")
    (sync-retiolum "go")
    (sync-retiolum "much")
    (sync-retiolum "newsbot-js")
    (sync-retiolum "populate")
    (sync-retiolum "stockholm")
    (sync-retiolum "wai-middleware-time")
    (sync-retiolum "web-routes-wai-custom")
    (sync-retiolum "xmonad-stockholm")
  ];
}
