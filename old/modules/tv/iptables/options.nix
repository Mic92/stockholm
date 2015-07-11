{ lib, ... }:

let
  inherit (lib) mkOption types;
in

{
  enable = mkOption {
    type = types.bool;
    default = false;
    description = "Enable iptables.";
  };

  input-internet-accept-new-tcp = mkOption {
    type = with types; listOf str;
    default = [];
    description = ''
      TCP ports, accepting incoming connections from anywhere.
    '';
  };

  input-retiolum-accept-new-tcp = mkOption {
    type = with types; listOf str;
    default = [];
    description = ''
      TCP ports, accepting incoming connections from Retiolum.
    '';
  };
}
