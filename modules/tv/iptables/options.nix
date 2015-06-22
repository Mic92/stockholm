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
      ip{4,6}tables -A INPUT -j ACCEPT -p tcp --dport $port -m conntrack --ctstate NEW
    '';
  };

  input-retiolum-accept-new-tcp = mkOption {
    type = with types; listOf str;
    default = [];
    description = ''
      ip{4,6}tables -A Retiolum -j ACCEPT -p tcp --dport $port -m conntrack --ctstate NEW
    '';
  };
}
