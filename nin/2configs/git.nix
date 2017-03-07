{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let

  out = {
    services.nginx.enable = true;
    krebs.git = {
      enable = true;
      cgit = {
        settings = {
          root-title = "public repositories at ${config.krebs.build.host.name}";
          root-desc = "keep calm and engage";
        };
      };
      repos = mapAttrs (_: s: removeAttrs s ["collaborators"]) repos;
      rules = rules;
    };

    krebs.iptables.tables.filter.INPUT.rules = [
      { predicate = "-i retiolum -p tcp --dport 80"; target = "ACCEPT"; }
    ];
  };

  repos = public-repos;

  rules = concatMap make-rules (attrValues repos);

  public-repos = mapAttrs make-public-repo {
    stockholm = {
      cgit.desc = "take all the computers hostage, they'll love you!";
    };
  };

  make-public-repo = name: { cgit ? {}, ... }: {
    inherit cgit name;
    public = true;
    hooks = {
      post-receive = pkgs.git-hooks.irc-announce {
        # TODO make nick = config.krebs.build.host.name the default
        nick = config.krebs.build.host.name;
        channel = "#retiolum";
        server = "ni.r";
        verbose = config.krebs.build.host.name == "onondaga";
        # TODO define branches in some kind of option per repo
        branches = [ "master" ];
      };
    };
  };

  make-rules =
    with git // config.krebs.users;
    repo:
      singleton {
        user = [ nin ];
        repo = [ repo ];
        perm = push "refs/*" [ non-fast-forward create delete merge ];
      } ++
      optional repo.public {
        user = attrValues config.krebs.users;
        repo = [ repo ];
        perm = fetch;
      } ++
      optional (length (repo.collaborators or []) > 0) {
        user = repo.collaborators;
        repo = [ repo ];
        perm = fetch;
      };

in out
