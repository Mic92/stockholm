{ config, lib, pkgs, ... }:

with import <stockholm/lib>;

let {

  body = {
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

  cgit-clear-cache = pkgs.cgit-clear-cache.override {
    inherit (config.krebs.git.cgit.settings) cache-root;
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
    dic = {
      cgit.desc = "dict.leo.org command line interface";
    };
    disko = {
      cgit.desc = "declarative partitioning and formatting tool";
    };
    get = {};
    hstool = {
      cgit.desc = "Haskell Development Environment ^_^";
    };
    htgen = {
      cgit.desc = "toy HTTP server";
    };
    kirk = {
      cgit.desc = "IRC tools";
    };
    krops = {
      cgit.desc = "deployment tools";
    };
    load-env = {};
    loldns = {
      cgit.desc = "toy DNS server";
    };
    make-snapshot = {};
    much = {};
    netcup = {
      cgit.desc = "netcup command line interface";
    };
    nix-writers = {};
    populate = {
      cgit.desc = "source code installer";
    };
    q = {};
    regfish = {};
    soundcloud = {
      cgit.desc = "SoundCloud command line interface";
    };
    stockholm = {
      cgit.desc = "NixOS configuration";
    };
  } // mapAttrs (_: recursiveUpdate { cgit.section = "2. Host configurations"; }) {
    ni = {
    };
  } // mapAttrs (_: recursiveUpdate { cgit.section = "3. Haskell libraries"; }) {
    blessings = {};
    mime = {};
    quipper = {};
    scanner = {};
    wai-middleware-time = {};
    web-routes-wai-custom = {};
    xintmap = {};
    xmonad-stockholm = {};
  } // mapAttrs (_: recursiveUpdate { cgit.section = "4. museum"; }) {
    cgserver = {};
    crude-mail-setup = {};
    dot-xmonad = {};
    hirc = {};
    make-snapshot = {};
    nixos-infest = {};
    painload = {};
    push = {};
    with-tmpdir = {};
  });

  restricted-repos = mapAttrs make-restricted-repo (
    {
      brain = {
        collaborators = with config.krebs.users; [ lass makefu ];
        hooks = {
          post-receive = /* sh */ ''
            (${irc-announce { cgit_endpoint = null; }})
            ${cgit-clear-cache}/bin/cgit-clear-cache
          '';
        };
      };
    } //
    # TODO don't put secrets/repos.nix into the store
    import <secrets/repos.nix> { inherit config lib pkgs; }
  );

  irc-announce = args: pkgs.git-hooks.irc-announce (recursiveUpdate {
    channel = "#xxx";
    # TODO make nick = config.krebs.build.host.name the default
    nick = config.krebs.build.host.name;
    server = "irc.r";
    verbose = true;
  } args);

  make-public-repo = name: { cgit ? {}, ... }: {
    inherit cgit name;
    public = true;
    hooks = {
      post-receive = /* sh */ ''
        (${optionalString (config.krebs.build.host.name == "ni")
                          (irc-announce {})})
        ${cgit-clear-cache}/bin/cgit-clear-cache
      '';
    };
  };

  make-restricted-repo = name: { collaborators ? [], hooks ? {}, ... }: {
    inherit collaborators name;
    public = false;
    hooks = hooks // {
      post-receive = /* sh */ ''
        (${hooks.post-receive or ""})
        ${cgit-clear-cache}/bin/cgit-clear-cache
      '';
    };
  };

  make-rules =
    with git // config.krebs.users;
    repo:
      singleton {
        user = [ tv tv-xu ];
        repo = [ repo ];
        perm = push "refs/*" [ non-fast-forward create delete merge ];
      } ++
      optional (repo.collaborators or [] != []) {
        user = repo.collaborators;
        repo = [ repo ];
        perm = fetch;
      };

}
