{ config, lib, pkgs, ... }:
with import ../../4lib/tv { inherit lib pkgs; };
let

  out = {
    imports = [ ../../3modules/krebs/git.nix ];
    krebs.git = {
      enable = true;
      root-title = "public repositories ";
      root-desc = "keep calm and enrage";
      inherit repos rules ;
    };
  };

  repos = public-repos;
  rules = concatMap make-rules (attrValues repos);

  public-repos = mapAttrs make-public-repo {
    stockholm = {
      desc = "take all the computers hostage, they'll love you!";
    };
  };

  # TODO move users to separate module

  make-public-repo = name: { desc ? null, ... }: {
    inherit name desc;
    public = true;
    hooks = {
      post-receive = git.irc-announce {
        nick = config.networking.hostName;
        channel = "#retiolum";
        server = "cd.retiolum";
      };
    };
  };

  make-rules =
    with git // config.krebs.users;
    repo:
      singleton {
        user = makefu;
        repo = [ repo ];
        perm = push "refs/*" [ non-fast-forward create delete merge ];
      } ++
      optional repo.public {
        user = [ lass tv uriel ];
        repo = [ repo ];
        perm = fetch;
      };

in out
