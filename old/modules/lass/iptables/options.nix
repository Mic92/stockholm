{ lib, ... }:

let
  inherit (lib) mkEnableOption mkOption types;
in

{
  enable = mkEnableOption "iptables";

  #tables.filter.INPUT = {
  # policy = "DROP";
  # rules = [
  #   { predicate = "-i retiolum"; target = "ACCEPT"; priority = -10; }
  # ];
  #};
  #new api
  tables = mkOption {
    type = with types; attrsOf (attrsOf (submodule ({
      options = {
        policy = mkOption {
          type = str;
          default = "-";
        };
        rules = mkOption {
          type = nullOr (listOf (submodule ({
            options = {
              predicate = mkOption {
                type = str;
              };
              target = mkOption {
                type = str;
              };
              precedence = mkOption {
                type = int;
                default = 0;
              };
            };
          })));
          default = null;
        };
      };
    })));
  };
}
