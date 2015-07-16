{ lib, ... }:

with lib;

{
  options = {
    sshKeys = mkOption {
      type = types.attrsOf (types.submodule (
      { config, ... }:
      {
        options = {
          pub = mkOption {
            type = types.str;
            description = "Public part of the ssh key.";
          };

          priv = mkOption {
            type = types.str;
            description = "Private part of the ssh key.";
          };
        };
      }));
      description = "collection of ssh-keys";
    };
  };
}
