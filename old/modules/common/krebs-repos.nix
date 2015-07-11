{ lib, ... }:

let
  inherit (lib) mkDefault;

  mkSecureRepo = name:
    { inherit name;
      value = {
        users = {
          lass = mkDefault "R";
          tv = mkDefault "R";
          makefu = mkDefault "R";
        };
      };
    };

  mkRepo = name:
    { inherit name;
      value = {
        users = {
          lass = mkDefault "R";
          tv = mkDefault "R";
          makefu = mkDefault "R";
        };
      };
    };

in {
  services.gitolite.repos =
    (lib.listToAttrs (map mkSecureRepo [ "brain" ])) //
    (lib.listToAttrs (map mkRepo [
      "painload"
      "services"
      "hosts"
    ]));
}
