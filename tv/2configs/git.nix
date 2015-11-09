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

  public-repos = mapAttrs make-public-repo ({
  } // mapAttrValues (setAttr "section" "1. Miscellaneous") {
    cac = {
      desc = "CloudAtCost command line interface";
    };
    get = {};
    hack = {};
    load-env = {};
    make-snapshot = {};
    much = {};
    nixpkgs = {};
    push = {};
    regfish = {};
    stockholm = {
      desc = "take all the computers hostage, they'll love you!";
    };
  } // mapAttrValues (setAttr "section" "2. Haskell libraries") {
    blessings = {};
    mime = {};
    quipper = {};
    scanner = {};
    wai-middleware-time = {};
    web-routes-wai-custom = {};
    xintmap = {};
    xmonad-stockholm = {};
  } // mapAttrValues (setAttr "section" "3. Museum") {
    cgserver = {};
    crude-mail-setup = {};
    dot-xmonad = {};
    nixos-infest = {};
    painload = {};
  });

  restricted-repos = mapAttrs make-restricted-repo (
    {
      brain = {
        collaborators = with config.krebs.users; [ lass makefu ];
      };
    } //
    # TODO don't put secrets/repos.nix into the store
    import <secrets/repos.nix> { inherit config lib pkgs; }
  );

  make-public-repo = name: { desc ? null, section ? null, ... }: {
    inherit name desc section;
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
        user = [ tv tv_xu ];
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
