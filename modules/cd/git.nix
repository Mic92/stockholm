{ config, lib, pkgs, ... }:

let
  inherit (builtins) map readFile;
  inherit (lib) concatMap listToAttrs;
  # TODO lib should already include our stuff
  inherit (import ../../lib { inherit lib pkgs; }) addNames git;

  cd-repos = [
    (public "cgserver")
    (public "crude-mail-setup")
    (public "dot-xmonad")
    (public "hack")
    (public "load-env")
    (public "make-snapshot")
    (public "mime")
    (public "much")
    (public "nixos-infest")
    (public "nixpkgs")
    (public "painload")
    (public "regfish")
    (public "repo")
    (public "shitment")
    (public "wai-middleware-time")
    (public "web-routes-wai-custom")
    (public "wu-configuration-nix")
  ];

  users = addNames {
    tv = { pubkey = readFile <pubkeys/tv.ssh.pub>; };
    lass = { pubkey = "xxx"; };
    makefu = { pubkey = "xxx"; };
  };

  repos = listToAttrs (map ({ repo, ... }: { name = repo.name; value = repo; }) cd-repos);

  rules = concatMap ({ rules, ... }: rules) cd-repos;

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
        { user = tv;
          repo = [ repo ];
          perm = push "refs/*" [ non-fast-forward create delete merge ];
        }
        { user = [ lass makefu ];
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

  services.git = {
    enable = true;
    inherit repos rules users;
  };
}
