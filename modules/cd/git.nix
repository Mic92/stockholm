{ config, lib, pkgs, ... }:

let
  inherit (builtins) readFile;
  # TODO lib should already include our stuff
  inherit (import ../../lib { inherit lib pkgs; }) addNames git;
in

{
  imports = [
    ../tv/git
  ];

  services.git = rec {
    enable = true;

    users = addNames {
      tv = { pubkey = readFile <pubkeys/tv.ssh.pub>; };
      lass = { pubkey = "xxx"; };
      makefu = { pubkey = "xxx"; };
    };

    repos = addNames {
      shitment = {
        desc = "shitment repository";
        hooks = {
          post-receive = git.irc-announce {
            nick = config.networking.hostName; # TODO make this the default
            channel = "#retiolum";
            server = "ire.retiolum";
          };
        };
        public = true;
      };
      testing = {
        desc = "testing repository";
        hooks = {
          post-receive = git.irc-announce {
            nick = config.networking.hostName; # TODO make this the default
            channel = "#retiolum";
            server = "ire.retiolum";
          };
        };
        public = true;
      };
    };

    rules = with git; with users; with repos; [
      { user = tv;
        repo = [ testing shitment ];
        perm = push "refs/*" [ non-fast-forward create delete merge ];
      }
      { user = [ lass makefu ];
        repo = [ testing shitment ];
        perm = fetch;
      }
    ];
  };
}
