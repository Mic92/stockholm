{ ... }:

{
  imports = [
    ../lass/gitolite-base.nix
    ../common/krebs-keys.nix
    ../common/krebs-repos.nix
  ];

  services.gitolite = {
    repos = {

      config = {
        users = {
          lass = "RW+";
          uriel = "R";
          tv = "R";
        };
        extraConfig = "option hook.post-receive = irc-announce";
      };

      pass = {
        users = {
          lass = "RW+";
          uriel = "R";
        };
      };

      load-env = {
        users = {
          lass = "RW+";
          uriel = "R";
          tv = "R";
        };
        extraConfig = "option hook.post-receive = irc-announce";
      };

      emse-hsdb = {
        users = {
          lass = "RW+";
          uriel = "R";
          tv = "R";
        };
        extraConfig = "option hook.post-receive = irc-announce";
      };

      brain = {
        users = {
          lass = "RW+";
        };
        extraConfig = "option hook.post-receive = irc-announce";
        #hooks.post-receive = irc-announce;
      };

      painload = {
        users = {
          lass = "RW+";
        };
        extraConfig = "option hook.post-receive = irc-announce";
      };

      services = {
        users = {
          lass = "RW+";
        };
        extraConfig = "option hook.post-receive = irc-announce";
      };

      xmonad-config = {
        users = {
          lass = "RW+";
          uriel = "R";
        };
      };

    };
  };
}
