{ config, pkgs, lib, ... }:
with lib;
let

  cfg = config.krebs.shadow;

  mergeShadowsJq = pkgs.writeJq "merge-shadows.jq" ''
    def is_int: . == (. | floor);
    def fields_4_to_9: ["", "", "", "", "", ""];
    def check_fields_3_to_9: (.[2] | tonumber | is_int) and .[3:] == fields_4_to_9;

    def read_value:
      split(":") |
      if length == 9 then
        if check_fields_3_to_9 then
          .
        else
          error("unrecognized field contents")
        end
      elif length == 2 then
        if .[1] | test("^\\$6\\$") then
          . + ["1"] + fields_4_to_9
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
      apply = x: if builtins.typeOf x == "path" then toString x else x;
      default = null;
      description = ''
        Path to a file containing additional shadow entries, used for adding
        encrypted passwords  which should not be placed into the Nix store.

        The overrides file may contain either regular shadow(5) entries like:

        <code>‹login-name›:‹hashed-password›:1::::::</code>

        Or shortened entries only containing login name and password like:

        <code>‹login-name›:‹hashed-password›</code>
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
