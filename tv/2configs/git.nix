{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let

  out = {
    krebs.git = {
      enable = true;
      cgit = {
        settings = {
          root-title = "repositories at ${config.krebs.build.host.name}";
          root-desc = "mostly krebs";
        };
      };
      repos = repos;
      rules = rules;
    };
  };

  repos =
    public-repos //
    optionalAttrs config.krebs.build.host.secure restricted-repos;

  rules = concatMap make-rules (attrValues repos);

  public-repos = mapAttrs make-public-repo ({
  } // mapAttrs (_: recursiveUpdate { cgit.section = "1. miscellaneous"; }) {
    cac-api = {
      cgit.desc = "CloudAtCost API command line interface";
    };
    get = {};
    hack = {};
    load-env = {};
    loldns = {
      cgit.desc = "toy DNS server";
    };
    make-snapshot = {};
    much = {};
    netcup = {
      cgit.desc = "netcup command line interface";
    };
    newsbot-js = {};
    nixpkgs = {};
    populate = {
      cgit.desc = "source code installer";
    };
    push = {};
    regfish = {};
    soundcloud = {
      cgit.desc = "SoundCloud command line interface";
    };
    stockholm = {
      cgit.desc = "NixOS configuration";
    };
    with-tmpdir = {};
  } // mapAttrs (_: recursiveUpdate { cgit.section = "2. Haskell libraries"; }) {
    blessings = {};
    mime = {};
    quipper = {};
    scanner = {};
    wai-middleware-time = {};
    web-routes-wai-custom = {};
    xintmap = {};
    xmonad-stockholm = {};
  } // mapAttrs (_: recursiveUpdate { cgit.section = "3. museum"; }) {
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

  make-public-repo = name: { cgit ? {}, ... }: {
    inherit cgit name;
    public = true;
    hooks = optionalAttrs (config.krebs.build.host.name == "ni") {
      post-receive = pkgs.git-hooks.irc-announce {
        # TODO make nick = config.krebs.build.host.name the default
        nick = config.krebs.build.host.name;
        channel = "#retiolum";
        server = "ni.r";
        verbose = true;
      };
    };
  };

  make-restricted-repo = name: { collaborators ? [], ... }: {
    inherit collaborators name;
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
        user = attrValues config.krebs.users;
        repo = [ repo ];
        perm = fetch;
      } ++
      optional (repo.collaborators or [] != []) {
        user = repo.collaborators;
        repo = [ repo ];
        perm = fetch;
      };

in out
