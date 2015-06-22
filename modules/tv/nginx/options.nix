{ lib, ... }:

let
  inherit (lib) mkOption types;
in

{
  enable = mkOption {
    type = types.bool;
    default = false;
    description = "Enable nginx.";
  };

  retiolum-locations = mkOption {
    type = with types; listOf (attrsOf str);
    default = [];
    description = ''
      TODO
    '';
  };
}
