{ config, lib, pkgs, ... }:

let
  inherit (builtins) map readFile;
  inherit (lib) concatMap listToAttrs;
  # TODO lib should already include our stuff
  inherit (import ../../lib { inherit lib pkgs; }) addNames git;

  x-repos = [
    (krebs-private "brain")

    (public "painload")
    (public "shitment")
    (public "wai-middleware-time")
    (public "web-routes-wai-custom")

    (secret "pass")

    (tv-lass "emse-drywall")
    (tv-lass "emse-hsdb")
  ];

  users = addNames {
    tv = { pubkey = readFile <pubkeys/tv_wu.ssh.pub>; };
    lass = { pubkey = readFile <pubkeys/lass.ssh.pub>; };
    uriel = { pubkey = readFile <pubkeys/uriel.ssh.pub>; };
    makefu = { pubkey = "xxx"; };
  };

  repos = listToAttrs (map ({ repo, ... }: { name = repo.name; value = repo; }) x-repos);

  rules = concatMap ({ rules, ... }: rules) x-repos;

  krebs-private = repo-name:
    rec {
      repo = {
        name = repo-name;
        hooks = {
          post-receive = git.irc-announce {
            nick = config.networking.hostName; # TODO make this the default
            channel = "#retiolum";
            server = "ire.retiolum";
          };
        };
      };
      rules = with git; with users; [
        { user = lass;
          repo = [ repo ];
          perm = push "refs/*" [ non-fast-forward create delete merge ];
        }
        { user = [ tv makefu uriel ];
          repo = [ repo ];
          perm = fetch;
        }
      ];
    };

  public = repo-name:
    rec {
      repo = {
        name = repo-name;
        hooks = {
          post-receive = git.irc-announce {
            nick = config.networking.hostName; # TODO make this the default
            channel = "#retiolum";
            server = "ire.retiolum";
          };
        };
        public = true;
      };
      rules = with git; with users; [
        { user = lass;
          repo = [ repo ];
          perm = push "refs/*" [ non-fast-forward create delete merge ];
        }
        { user = [ tv makefu uriel ];
          repo = [ repo ];
          perm = fetch;
        }
      ];
    };

  secret = repo-name:
    rec {
      repo = {
        name = repo-name;
        hooks = {};
      };
      rules = with git; with users; [
        { user = lass;
          repo = [ repo ];
          perm = push "refs/*" [ non-fast-forward create delete merge ];
        }
        { user = [ uriel ];
          repo = [ repo ];
          perm = fetch;
        }
      ];
    };

  tv-lass = repo-name:
    rec {
      repo = {
        name = repo-name;
        hooks = {};
      };
      rules = with git; with users; [
        { user = lass;
          repo = [ repo ];
          perm = push "refs/*" [ non-fast-forward create delete merge ];
        }
        { user = [ tv ];
          repo = [ repo ];
          perm = fetch;
        }
      ];
    };

in

{
  imports = [
    ../tv/git
  ];

  tv.git = {
    enable = true;
    inherit repos rules users;
  };
}
