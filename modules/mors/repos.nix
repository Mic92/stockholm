{ ... }:

{
  imports = [
    ../lass/gitolite-base.nix
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

      painload = {
        users = {
          lass = "RW+";
          tv = "R";
          makefu = "R";
        };
        extraConfig = "option hook.post-receive = irc-announce";
      };

      brain = {
        users = {
          lass = "RW+";
          tv = "R";
          makefu = "R";
        };
        extraConfig = "option hook.post-receive = irc-announce";
      };

      services = {
        users = {
          lass = "RW+";
          tv = "R";
          makefu = "R";
          reaktor = "R";
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
