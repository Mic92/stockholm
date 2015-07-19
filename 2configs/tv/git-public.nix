{ config, lib, pkgs, ... }:

with lib;
let
  inherit (builtins) map readFile;
  inherit (lib) concatMap listToAttrs;
  # TODO lib should already include our stuff
  inherit (import ../../4lib/tv { inherit lib pkgs; }) addNames git;

  public-git-repos = [
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
    (public "quipper")
    (public "regfish")
    (public' {
      name = "shitment";
      desc = "turn all the computers into one computer!";
    })
    (public "wai-middleware-time")
    (public "web-routes-wai-custom")
    (public "xintmap")
  ];

  users = addNames {
    tv = { pubkey = readFile ../../Zpubkeys/tv_wu.ssh.pub; };
    lass = { pubkey = readFile ../../Zpubkeys/lass.ssh.pub; };
    uriel = { pubkey = readFile ../../Zpubkeys/uriel.ssh.pub; };
    makefu = { pubkey = readFile ../../Zpubkeys/makefu.ssh.pub; };
  };

  repos = listToAttrs (map ({ repo, ... }: { name = repo.name; value = repo; }) public-git-repos);

  rules = concatMap ({ rules, ... }: rules) public-git-repos;

  public' = { name, desc }:
    let
      x = public name;
    in
    x // { repo = x.repo // { inherit desc; }; };

  public = repo-name:
    rec {
      repo = {
        name = repo-name;
        hooks = {
          post-receive = git.irc-announce {
            nick = config.networking.hostName; # TODO make this the default
            channel = "#retiolum";
            server = "cd.retiolum";
          };
        };
        public = true;
      };
      rules = with git; with users; [
        { user = tv;
          repo = [ repo ];
          perm = push "refs/*" [ non-fast-forward create delete merge ];
        }
        { user = [ lass makefu uriel ];
          repo = [ repo ];
          perm = fetch;
        }
      ];
    };

in

{
  imports = [
    ../../3modules/tv/git.nix
  ];
  tv.git = {
    enable = true;
    inherit repos rules users;
    root-title = "public repositories at ${config.networking.hostName}";
    root-desc = "keep calm and engage";
  };
}
