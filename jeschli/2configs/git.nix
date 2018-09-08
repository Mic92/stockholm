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
        enable = true;
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
      cgit.desc = "Bonbon aus Git - die ganze Nacht";
    };
    krebs-page = {
      cgit.desc = "Die Krebs Page";
    };
  };

  make-public-repo = name: { cgit ? {}, ... }: {
    inherit cgit name;
    public = true;
    hooks = {
      post-receive = pkgs.git-hooks.irc-announce {
        channel = "#xxx";
        nick = config.krebs.build.host.name;
        refs = [
          "refs/heads/master"
        ];
        server = "irc.r";
        verbose = true;
      };
    };
  };

  make-rules =
    with git // config.krebs.users;
    repo:
      singleton {
        user = [ jeschli jeschli-brauerei];
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
