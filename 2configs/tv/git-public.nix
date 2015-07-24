{ config, lib, pkgs, ... }:
with import ../../4lib/tv { inherit lib pkgs; };
let

  out = {
    krebs.git = {
      enable = true;
      root-title = "public repositories at ${config.tv.identity.self.name}";
      root-desc = "keep calm and engage";
      inherit repos rules users;
    };
  };

  repos = public-repos;
  rules = concatMap make-rules (attrValues repos);

  public-repos = mapAttrs make-public-repo {
    cgserver = {};
    crude-mail-setup = {};
    dot-xmonad = {};
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

  # TODO move users to separate module
  users = mapAttrs make-user {
    tv = ../../Zpubkeys/tv_wu.ssh.pub;
    lass = ../../Zpubkeys/lass.ssh.pub;
    uriel = ../../Zpubkeys/uriel.ssh.pub;
    makefu = ../../Zpubkeys/makefu.ssh.pub;
  };

  make-public-repo = name: { desc ? null, ... }: {
    inherit name desc;
    public = true;
    hooks = {
      post-receive = git.irc-announce {
        # TODO make nick = config.tv.identity.self.name the default
        nick = config.tv.identity.self.name;
        channel = "#retiolum";
        server = "cd.retiolum";
      };
    };
  };

  make-rules =
    with git // users;
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
      };

  make-user = name: pubkey-file: {
    inherit name;
    pubkey = readFile pubkey-file;
  };

in out
