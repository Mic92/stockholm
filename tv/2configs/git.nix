{ config, lib, pkgs, ... }:

with config.krebs.lib;

let

  out = {
    krebs.git = {
      enable = true;
      root-title = "repositories at ${config.krebs.build.host.name}";
      root-desc = "mostly krebs";
      repos = repos;
      rules = rules;
    };
  };

  repos =
    public-repos //
    optionalAttrs config.krebs.build.host.secure restricted-repos;

  rules = concatMap make-rules (attrValues repos);

  public-repos = mapAttrs make-public-repo ({
  } // mapAttrValues (setAttr "section" "1. miscellaneous") {
    cac-api = {
      desc = "CloudAtCost API command line interface";
    };
    get = {};
    hack = {};
    load-env = {};
    make-snapshot = {};
    much = {};
    newsbot-js = {};
    nixpkgs = {};
    push = {};
    regfish = {};
    soundcloud = {
      desc = "SoundCloud command line interface";
    };
    stockholm = {
      desc = "NixOS configuration";
    };
    with-tmpdir = {};
  } // mapAttrValues (setAttr "section" "2. Haskell libraries") {
    blessings = {};
    mime = {};
    quipper = {};
    scanner = {};
    wai-middleware-time = {};
    web-routes-wai-custom = {};
    xintmap = {};
    xmonad-stockholm = {};
  } // mapAttrValues (setAttr "section" "3. museum") {
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
    hooks = optionalAttrs (config.krebs.build.host.name == "cd") {
      post-receive = pkgs.git-hooks.irc-announce {
        # TODO make nick = config.krebs.build.host.name the default
        nick = config.krebs.build.host.name;
        channel = "#retiolum";
        server = "cd.retiolum";
        verbose = true;
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
        user = [ tv tv-xu ];
        repo = [ repo ];
        perm = push "refs/*" [ non-fast-forward create delete merge ];
      } ++
      optional repo.public {
        user = [ lass makefu ];
        repo = [ repo ];
        perm = fetch;
      } ++
      optional (repo.collaborators or [] != []) {
        user = repo.collaborators;
        repo = [ repo ];
        perm = fetch;
      };

in out
