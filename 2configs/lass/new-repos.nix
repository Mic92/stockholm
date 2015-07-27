{ config, lib, pkgs, ... }:

with import ../../4lib/tv { inherit lib pkgs; };
let

  out = {
    krebs.git = {
      enable = true;
      root-title = "public repositories at ${config.krebs.build.host.name}";
      root-desc = "keep calm and engage";
      inherit repos rules;
    };
  };

  repos = mapAttrs (_: s: removeAttrs s ["collaborators"]) (
    public-repos //
    optionalAttrs config.krebs.build.host.secure restricted-repos
  );

  rules = concatMap make-rules (attrValues repos);

  public-repos = mapAttrs make-public-repo {
    painload = {};
    stockholm = {
      desc = "take all the computers hostage, they'll love you!";
    };
    wai-middleware-time = {};
    web-routes-wai-custom = {};
  };

  restricted-repos = mapAttrs make-restricted-repo (
    {
      brain = {
        collaborators = with config.krebs.users; [ tv makefu ];
      };
    } //
    import /root/src/secrets/repos.nix { inherit config lib pkgs; }
  );

  make-public-repo = name: { desc ? null, ... }: {
    inherit name desc;
    public = true;
    hooks = {
      post-receive = git.irc-announce {
        # TODO make nick = config.krebs.build.host.name the default
        nick = config.krebs.build.host.name;
        channel = "#retiolum";
        server = "cd.retiolum";
      };
    };
  };

  make-restricted-repo = name: { desc ? null, ... }: {
    inherit name desc;
    public = false;
  };

  make-rules =
    with git // config.krebs.users;
    repo:
      singleton {
        user = lass;
        repo = [ repo ];
        perm = push "refs/*" [ non-fast-forward create delete merge ];
      } ++
      optional repo.public {
        user = [ tv makefu uriel ];
        repo = [ repo ];
        perm = fetch;
      } ++
      optional (length (repo.collaborators or []) > 0) {
        user = repo.collaborators;
        repo = [ repo ];
        perm = fetch;
      };

in out
