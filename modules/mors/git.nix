{ config, lib, pkgs, ... }:

{
  imports = [
    ../tv/git
  ];

  services.git =
    let
      inherit (builtins) readFile;
      # TODO lib should already include our stuff
      inherit (import ../../lib { inherit lib pkgs; }) addNames git;

      krebs-private = name: desc:
          {
            inherit desc;
            hooks = {
              post-receive = git.irc-announce {
                nick = config.networking.hostName; # TODO make this the default
                channel = "#retiolum";
                server = "ire.retiolum";
              };
            };
          }
    in rec {
      enable = true;

      users = addNames {
        tv = { pubkey = readFile <pubkeys/tv.ssh.pub>; };
        lass = { pubkey = readFile <pubkeys/lass.ssh.pub>; };
        uriel = { pubkey = readFile <pubkeys/lass.ssh.pub>; };
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
              channel = "#repository";
              server = "ire.retiolum";
            };
          };
          public = true;
        };
      };

      rules = with git; with users; with repos; [
        { user = lass;
          repo = [ testing shitment ];
          perm = push master [ non-fast-forward create delete merge ];
        }
        { user = [ tv uriel makefu ];
          repo = [ testing shitment ];
          perm = fetch;
        }
      ];
    };
}
