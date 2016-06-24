{ config, lib, pkgs, ... }:

with config.krebs.lib;

let
  mirror = "git@${config.networking.hostName}:";

  sync = name: let
    repo = {
      public = true;
      name = mkDefault "${name}";
      cgit.desc = mkDefault "mirror for all ${name} branches";
      hooks = mkDefault {
        post-receive = pkgs.git-hooks.irc-announce {
          nick = config.networking.hostName;
          verbose = false;
          channel = "#retiolum";
          server = "cd.retiolum";
        };
      };
    };
  in {
    krebs.repo-sync.repos.${name} = {
      makefu = {
        origin.url = "http://cgit.gum/${name}";
        mirror.url = "${mirror}${name}";
      };
      tv = {
        origin.url = "http://cgit.cd/${name}";
        mirror.url = "${mirror}${name}";
      };
      lassulus = {
        origin.url = "http://cgit.prism/${name}";
        mirror.url = "${mirror}${name}";
      };
      "@latest" = {
        mirror.url = "${mirror}${name}";
        mirror.ref = "heads/newest";
      };
    };
    krebs.git = {
      rules = with git; singleton {
        user = [ config.krebs.users."${config.networking.hostName}-repo-sync" ];
        repo = [ repo ];
        perm = push ''refs/*'' [ non-fast-forward create delete merge ];
      };
      repos."${name}" = repo;
    };
  };

in {
  krebs.repo-sync = {
    enable = true;
    privateKeyFile = toString <secrets/repo-sync.key>;
  };
  imports = [
    (sync "stockholm")
    (sync "realwallpaper")
    (sync "xmonad-stockholm")
    (sync "newsbot-js")
    (sync "go")
    (sync "wai-middleware-time")
    (sync "web-routes-wai-custom")
    (sync "much")
    (sync "painload")
  ];
}

