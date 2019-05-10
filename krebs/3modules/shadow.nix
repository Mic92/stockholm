with import <stockholm/lib>;
{ config, pkgs, ... }: let

  cfg = config.krebs.shadow;

  mergeShadowsJq = pkgs.writeJq "merge-shadows.jq" ''
    def fields_3_to_9: ["1", "", "", "", "", "", ""];

    def read_value:
      split(":") |
      if length == 9 then
        if .[2:] == fields_3_to_9 then
          .
        else
          error("unrecognized field contents")
        end
      elif length == 2 then
        if .[1] | test("^\\$6\\$") then
          . + fields_3_to_9
        else
          error("unrecognized hashed password")
        end
      else
        error("unexpected field count: expected 9 or 2, got \(length)")
      end;

    def write_value:
      join(":");

    split("\n") |
    map(select(length > 0) | read_value) |

    reverse |
    unique_by(.[0]) |
    map(write_value) |
    sort |

    join("\n")
  '';

in {

  options.krebs.shadow = {
    enable = mkEnableOption "krebs.shadow" // {
      default = cfg.overridesFile != null;
    };
    overridesFile = mkOption {
      apply = x: if typeOf x == "path" then toString x else x;
      default = null;
      description = ''
        Path to a file containing additional shadow entries, used for adding
        encrypted passwords  which should not be placed into the Nix store.

        The overrides file may contain either regular shadow(5) entries like:

        <code>&lt;login-name&gt;:&lt;hashed-password&gt;:1::::::</code>

        Or shortened entries only containing login name and password like:

        <code>&lt;login-name&gt;:&lt;hashed-password&gt</code>
      '';
      type = types.nullOr (types.either types.path types.absolute-pathname);
    };
  };

  config = let
  in mkIf cfg.enable {
    system.activationScripts.users-tv = stringAfter [ "users" ] /* sh */ ''
      (
        set -efu
        umask 77
        ${pkgs.jq}/bin/jq -Rrs -f ${mergeShadowsJq} \
            /etc/shadow ${cfg.overridesFile} > /etc/shadow~
        ${pkgs.coreutils}/bin/mv /etc/shadow /etc/shadow-
        ${pkgs.coreutils}/bin/mv /etc/shadow~ /etc/shadow
      )
    '';
  };
}
