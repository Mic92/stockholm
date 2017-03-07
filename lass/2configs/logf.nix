{ config, pkgs, ... }:
with import <stockholm/lib>;
let
  host-colors = {
    mors = "131";
    prism = "95";
    uriel = "61";
    shodan = "51";
    icarus = "53";
    echelon = "197";
    helios = "199";
    cloudkrebs = "119";
  };
in {
  environment.systemPackages = [
    (pkgs.writeDashBin "logf" ''
      export LOGF_HOST_COLORS=${pkgs.writeJSON "host-colors" host-colors}
      ${pkgs.logf}/bin/logf ${concatMapStringsSep " " (name: "root@${name}") (attrNames config.lass.hosts)}
    '')
  ];
}
