{ lib, ... }: 

let
  inherit (lib) literalExample mkOption types;
in

{
  enable = mkOption {
    type = types.bool;
    default = false;
    description = "Enable Git repository hosting.";
  };
  cgit = mkOption {
    type = types.bool;
    default = true;
    description = "Enable cgit."; # TODO better desc; talk about nginx
  };
  dataDir = mkOption {
    type = types.str;
    default = "/var/lib/git";
    description = "Directory used to store repositories.";
  };
  etcDir = mkOption {
    type = types.str;
    default = "/etc/git";
  };
  rules = mkOption {
    type = types.unspecified;
  };
  repos = mkOption {
    type = types.attrsOf (types.submodule ({
      options = {
        desc = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = ''
            Repository description.
          '';
        };
        section = mkOption {
          type = types.nullOr types.str;
          default = null;
          description = ''
            Repository section.
          '';
        };
        name = mkOption {
          type = types.str;
          description = ''
            Repository name.
          '';
        };
        hooks = mkOption {
          type = types.attrsOf types.str;
          description = ''
            Repository-specific hooks.
          '';
        };
        public = mkOption {
          type = types.bool;
          default = false;
          description = ''
            Allow everybody to read the repository via HTTP if cgit enabled.
          '';
          # TODO allow every configured user to fetch the repository via SSH.
        };
      };
    }));

    default = {};

    example = literalExample ''
      {
        testing = {
          name = "testing";
          hooks.post-update = '''
            #! /bin/sh
            set -euf
            echo post-update hook: $* >&2
          ''';
        };
        testing2 = { name = "testing2"; };
      }
    '';

    description = ''
      Repositories.
    '';
  };
  users = mkOption {
    type = types.unspecified;
  };
}
