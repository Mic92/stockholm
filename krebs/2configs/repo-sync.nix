{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let
  konsens-user = {
    name = "konsens";
    pubkey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIKKozGNGBAzHnyj6xUlsjGxxknyChXvuyrddkWVVnz7";
  };
  mirror = "git@${config.networking.hostName}:";

  defineRepo = {
    name, desc, section
  }:
  let
    repo = {
      public = true;
      name = mkDefault "${name}";
      cgit.desc = desc;
      cgit.section = section;
      hooks = mkDefault {
        post-receive = pkgs.git-hooks.irc-announce {
          channel = "#xxx";
          refs = [
            "refs/heads/master"
            "refs/heads/newest"
            "refs/tags/*"
          ];
          nick = config.networking.hostName;
          server = "irc.r";
          verbose = false;
        };
      };
    };
  in {
    rules = with git; [
      {
        user = with config.krebs.users; [
          config.krebs.users."${config.networking.hostName}-repo-sync"
          jeschli
          lass
          makefu
          tv
        ];
        repo = [ repo ];
        perm = push ''refs/*'' [ non-fast-forward create delete merge ];
      }
      {
        user = [
          konsens-user
        ];
        repo = [ repo ];
        perm = push ''refs/heads/master'' [ create merge ];
      }
      {
        user = attrValues config.krebs.users;
        repo = [ repo ];
        perm = fetch;
      }
    ];
    repos."${name}" = repo;
  };

  sync-retiolum = {
    name,
    desc ? "mirror for ${name}",
    section ? "mirror"
  }:
    {
      krebs.repo-sync.repos.${name} = {
        branches = {
          lassulus = {
            origin.url = "http://cgit.lassul.us/${name}";
            mirror.url = "${mirror}${name}";
          };
          makefu = {
            origin.url = "http://cgit.gum/${name}";
            mirror.url = "${mirror}${name}";
          };
          nin = {
            origin.url = "http://cgit.onondaga.r/${name}";
            mirror.url = "${mirror}${name}";
          };
          tv = {
            origin.url = "http://cgit.ni.r/${name}";
            mirror.url = "${mirror}${name}";
          };
        };
        latest = {
          url = "${mirror}${name}";
          ref = "heads/newest";
        };
      };
      krebs.git = defineRepo { inherit name desc section; };
    };

  sync-remote = {
    name,
    url,
    desc ? "mirror for ${name}",
    section ? "mirror"
  }:
    {
      krebs.repo-sync.repos.${name} = {
        branches = {
          remote = {
            origin.url = url;
            mirror.url = "${mirror}${name}";
          };
        };
      };
      krebs.git = defineRepo { inherit name desc section; };
    };

in {
  krebs.git = {
    enable = true;
    cgit.settings = {
      root-title = "krebs repos";
      root-desc = "keep calm and engage";
    };
  };
  krebs.repo-sync = {
    enable = true;
  };
  krebs.konsens = {
    enable = true;
    repos = {
      krops = { branchesToCheck = [ "lassulus" "tv" ]; };
      stockholm = {};
    };
  };
  krebs.secret.files.konsens = {
    path = "/var/lib/konsens/.ssh/id_ed25519";
    owner = konsens-user;
    source-path = "${<secrets/konsens.id_ed25519>}";
  };

  imports = [
    (sync-retiolum { name = "the_playlist"; desc = "Good Music collection + tools"; section = "art"; })

    (sync-retiolum { name = "stockholm"; desc = "take all computers hostage, they love it"; section = "configuration"; })

    (sync-retiolum { name = "cholerab"; desc = "krebs thesauron & enterprise-patterns"; section = "documentation"; })

    (sync-retiolum { name = "disko"; desc = "take a description of your disk layout and produce a format script"; section = "software"; })
    (sync-retiolum { name = "news"; desc = "take a rss feed and a timeout and print it to stdout"; section = "software"; })
    (sync-retiolum { name = "krops"; desc = "krebs ops"; section = "software"; })
    (sync-retiolum { name = "go"; desc = "url shortener"; section = "software"; })
    (sync-retiolum { name = "much"; desc = "curses email client"; section = "software"; })
    (sync-retiolum { name = "newsbot-js"; desc = "irc rss/atom bot"; section = "software"; })
    (sync-retiolum { name = "nix-writers"; desc = "high level writers for nix"; section = "software"; })

    (sync-retiolum { name = "cac-api"; desc = "CloudAtCost API command line interface"; section = "miscellaneous"; })
    (sync-retiolum { name = "dic"; desc = "dict.leo.org command line interface"; section = "miscellaneous"; })
    (sync-retiolum { name = "get"; section = "miscellaneous"; })
    (sync-retiolum { name = "hstool"; desc = "Haskell Development Environment ^_^"; section = "miscellaneous"; })
    (sync-retiolum { name = "htgen"; desc = "toy HTTP server"; section = "miscellaneous"; })
    (sync-retiolum { name = "kirk"; desc = "IRC tools"; section = "miscellaneous"; })
    (sync-retiolum { name = "load-env"; section = "miscellaneous"; })
    (sync-retiolum { name = "loldns"; desc = "toy DNS server"; section = "miscellaneous"; })
    (sync-retiolum { name = "netcup"; desc = "netcup command line interface"; section = "miscellaneous"; })
    (sync-retiolum { name = "populate"; desc = "source code installer"; section = "miscellaneous"; })
    (sync-retiolum { name = "q"; section = "miscellaneous"; })
    (sync-retiolum { name = "regfish"; section = "miscellaneous"; })
    (sync-retiolum { name = "soundcloud"; desc = "SoundCloud command line interface"; section = "miscellaneous"; })

    (sync-retiolum { name = "blessings"; section = "Haskell libraries"; })
    (sync-retiolum { name = "mime"; section = "Haskell libraries"; })
    (sync-retiolum { name = "quipper"; section = "Haskell libraries"; })
    (sync-retiolum { name = "scanner"; section = "Haskell libraries"; })
    (sync-retiolum { name = "wai-middleware-time"; section = "Haskell libraries"; })
    (sync-retiolum { name = "web-routes-wai-custom"; section = "Haskell libraries"; })
    (sync-retiolum { name = "xintmap"; section = "Haskell libraries"; })
    (sync-retiolum { name = "xmonad-stockholm"; desc = "krebs xmonad modules"; section = "Haskell libraries"; })

    (sync-remote { name = "array"; url = "https://github.com/makefu/array"; })
    (sync-remote { name = "email-header"; url = "https://github.com/4z3/email-header"; })
    (sync-remote { name = "mycube-flask"; url = "https://github.com/makefu/mycube-flask"; })
    (sync-remote { name = "reaktor-titlebot"; url = "https://github.com/makefu/reaktor-titlebot"; })
    (sync-remote { name = "repo-sync"; url = "https://github.com/makefu/repo-sync"; })
    (sync-remote { name = "skytraq-datalogger"; url = "https://github.com/makefu/skytraq-datalogger"; })
    (sync-remote { name = "realwallpaper"; url = "https://github.com/lassulus/realwallpaper"; })
    (sync-remote { name = "painload"; url = "https://github.com/krebs/painload"; })
    (sync-remote { name = "Reaktor"; url = "https://github.com/krebs/Reaktor"; })
    (sync-remote { name = "nixos-wiki"; url = "https://github.com/Mic92/nixos-wiki.wiki.git"; })
  ];
}
