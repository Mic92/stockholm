{ config, lib, pkgs, ... }:

with import ../4lib { inherit lib pkgs; };
let

  out = {
    krebs.git = {
      enable = true;
      root-title = "public repositories at ${config.krebs.build.host.name}";
      root-desc = "keep calm and engage";
      repos = mapAttrs (_: s: removeAttrs s ["collaborators"]) repos;
      rules = rules;
    };
  };

  repos =
    public-repos //
    optionalAttrs config.krebs.build.host.secure restricted-repos;

  rules = concatMap make-rules (attrValues repos);

  public-repos = mapAttrs make-public-repo {
    cac = {
      desc = "CloudAtCost command line interface";
    };
    cgserver = {};
    crude-mail-setup = {};
    dot-xmonad = {};
    get = {};
    hack = {};
    load-env = {};
    make-snapshot = {};
    mime = {};
    much = {};
    nixos-infest = {};
    nixpkgs = {};
    painload = {};
    quipper = {};
    regfish = {};
    stockholm = {
      desc = "take all the computers hostage, they'll love you!";
    };
    wai-middleware-time = {};
    web-routes-wai-custom = {};
    xintmap = {};
  };

  restricted-repos = mapAttrs make-restricted-repo (
    {
      brain = {
        collaborators = with config.krebs.users; [ lass makefu ];
      };
    } //
    # TODO don't put secrets/repos.nix into the store
    import <secrets/repos.nix> { inherit config lib pkgs; }
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
        verbose = config.krebs.build.host.name == "cd";
      };
    };
  };

  make-restricted-repo = name: { collaborators ? [], desc ? null, ... }: {
    inherit name collaborators desc;
    public = false;
  };

  make-rules =
    with git // config.krebs.users;
    repo:
      singleton {
        user = tv;
        repo = [ repo ];
        perm = push "refs/*" [ non-fast-forward create delete merge ];
      } ++
      optional repo.public {
        user = [ lass makefu uriel ];
        repo = [ repo ];
        perm = fetch;
      } ++
      optional (length (repo.collaborators or []) > 0) {
        user = repo.collaborators;
        repo = [ repo ];
        perm = fetch;
      };

in out
