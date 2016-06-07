{ config, lib, pkgs, ... }:

with config.krebs.lib;

let

  out = {
    krebs.git = {
      enable = true;
      cgit = {
        settings = {
          root-title = "public repositories at ${config.krebs.build.host.name}";
          root-desc = "Hmhmh, im Moment nicht.";
        };
      };
      repos = mapAttrs (_: s: removeAttrs s ["collaborators"]) repos;
      rules = rules;
    };
  };

  repos = public-repos;

  rules = concatMap make-rules (attrValues repos);

  public-repos = mapAttrs make-public-repo {
    stockholm = {};
  };

  make-public-repo = name: { cgit ? {}, ... }: {
    inherit cgit name;
    public = true;
    hooks = {
      post-receive = pkgs.git-hooks.irc-announce {
        # TODO make nick = config.krebs.build.host.name the default
        nick = config.krebs.build.host.name;
        channel = "#retiolum";
        server = "cd.retiolum";
        verbose = config.krebs.build.host.name == "stro";
      };
    };
  };

  make-rules =
    with git // config.krebs.users;
    repo:
      singleton {
        user = [ mv_stro ];
        repo = [ repo ];
        perm = push "refs/*" [ non-fast-forward create delete merge ];
      } ++
      optional repo.public {
        user = [ lass makefu uriel tv tv-xu ];
        repo = [ repo ];
        perm = fetch;
      } ++
      optional (length (repo.collaborators or []) > 0) {
        user = repo.collaborators;
        repo = [ repo ];
        perm = fetch;
      };

in out
